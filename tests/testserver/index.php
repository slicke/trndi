<?php
// Fake NightScout for testing

function generateRecord() {
    // 1. Generate unique identifier (similar to MongoDB ObjectId)
    $identifier = generateObjectId();

    // 2. Get current Unix timestamp in milliseconds
    $timestampMs = round(microtime(true) * 1000);

    // 3. Convert timestamp to ISO 8601 format
    $isoDate = convertMillisecondsToISO8601($timestampMs);

    return [
        'identifier' => $identifier,
        'timestamp_ms' => $timestampMs,
        'iso_date' => $isoDate
    ];
}

/**
 * Genererar en unik identifierare liknande MongoDB ObjectId.
 *
 * @return string 24-tecken lång hex-sträng.
 * @throws Exception Vid fel vid generering av slumpmässiga bytes.
 */
function generateObjectId() {
    // 4 bytes for the timestamp (seconds since epoch)
    $time = dechex(time());
    $time = str_pad($time, 8, '0', STR_PAD_LEFT);

    // 5 random bytes
    $random = bin2hex(random_bytes(5));

    // 3 byte counter (initialized randomly)
    $counter = dechex(mt_rand(0, 0xFFFFFF));
    $counter = str_pad($counter, 6, '0', STR_PAD_LEFT);

    return $time . $random . $counter;
}

/**
 * Konverterar Unix-tidsstämpel i millisekunder till ISO 8601-format.
 *
 * @param int|string $milliseconds Unix-tidsstämpel i millisekunder.
 * @return string ISO 8601-formaterad datumsträng.
 */
function convertMillisecondsToISO8601($milliseconds) {
    // If the timestamp is a string (for large numbers on 32-bit systems)
    if (is_string($milliseconds)) {
        $seconds = bcdiv($milliseconds, '1000', 0);
        $millis = bcmod($milliseconds, '1000');
    } else {
        $seconds = floor($milliseconds / 1000);
        $millis = $milliseconds % 1000;
    }

    // Create DateTime object from seconds
    $date = new DateTime("@$seconds");
    $date->setTimezone(new DateTimeZone('UTC')); // Set timezone to UTC

    // Add milliseconds
    // PHP DateTime only supports microseconds, so we adjust accordingly
    $date->modify("+{$millis} milliseconds");

    // Formatera till ISO 8601 med millisekunder
    // 'v' ger millisekunder i DateTime => =>format
    return $date->format('Y-m-d\TH:i:s.v\Z');
}

$entries = [];
$str = "";

// Normalize the request path so that double-slashes (e.g. /api/v1//status.json)
// are treated the same as single slashes. This makes the test server more
// tolerant to minor client variations.
$raw_path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
$path = preg_replace('#/+#', '/', $raw_path);

// Log requests for debugging (writes to /tmp/trndi-test-server.log)
$logEntry = sprintf("[%s] %s %s (normalized: %s) - API-SECRET: %s\n",
    date('Y-m-d H:i:s'),
    $_SERVER['REQUEST_METHOD'],
    $raw_path,
    $path,
    isset($_SERVER['HTTP_API_SECRET']) ? substr($_SERVER['HTTP_API_SECRET'], 0, 10) . '...' : 'NONE'
);
file_put_contents('/tmp/trndi-test-server.log', $logEntry, FILE_APPEND);

// Debug endpoint to see what path is being requested
if ($path == '/debug') {
    header('Content-Type: text/plain');
    echo "Requested path: " . $path . "\n";
    echo "Raw path: " . $raw_path . "\n";
    echo "Request URI: " . $_SERVER['REQUEST_URI'] . "\n";
    print_r($_SERVER);
    exit;
}

// -----------------------------------------------------------------------------
// Fake Dexcom Share API for testing
// -----------------------------------------------------------------------------
// Dexcom client base URL ends with: /ShareWebServices/Services
// so endpoints look like:
//   /ShareWebServices/Services/General/LoginPublisherAccountByName
//   /ShareWebServices/Services/General/SystemUtcTime
//   /ShareWebServices/Services/Publisher/ReadPublisherLatestGlucoseValues
//
// The Dexcom client in Trndi uses a JSON POST body; keep parsing permissive.
function getJsonBody() {
    $raw = file_get_contents('php://input');
    if ($raw === false || trim($raw) === '') return [];
    $decoded = json_decode($raw, true);
    return is_array($decoded) ? $decoded : [];
}

if (str_starts_with($path, '/ShareWebServices/Services/')) {
    header('Content-Type: application/json');

    $sub = substr($path, strlen('/ShareWebServices/Services/'));
    $body = getJsonBody();

    if ($sub === 'General/LoginPublisherAccountByName') {
        // Return a deterministic non-empty session token.
        echo json_encode('TEST-DEXCOM-SESSION');
        exit;
    }

    if ($sub === 'General/SystemUtcTime') {
        // Dexcom Share often returns a JSON string like "/Date(1690000000000)/".
        $ms = (int) floor(microtime(true) * 1000);
        // NOTE: json_encode will escape slashes ("\/Date(... )\/") which breaks
        // the strict Pascal string slicing in Dexcom.GetReadings.
        echo '"/Date(' . $ms . ')/"';
        exit;
    }

    if ($sub === 'Publisher/ReadPublisherLatestGlucoseValues') {
        // Return a minimal array of readings. Fields picked to match common
        // Dexcom Share JSON.
        $nowMs = (int) floor(microtime(true) * 1000);
        $items = [];
        for ($i = 0; $i < 3; $i++) {
            $t = ($nowMs - ($i * 5 * 60 * 1000));
            $items[] = [
                // NOTE: Trndi's current Dexcom parser slices with Copy(S, 6, ..)
                // and then StrToInt64. That only works if the string is exactly
                // "/Date" + digits + ")/" (no opening parenthesis after Date).
                'WT' => '/Date' . $t . ')/',
                'ST' => '/Date' . $t . ')/',
                'Value' => 120 + $i,
                'Trend' => 'Flat'
            ];
        }
        // Emit JSON without escaping forward slashes so the client sees
        // "/Date(ms)/" (not "\/Date(ms)\/").
        echo json_encode($items, JSON_UNESCAPED_SLASHES);
        exit;
    }

    http_response_code(404);
    echo json_encode(['error' => 'Unknown Dexcom endpoint', 'path' => $sub]);
    exit;
}

// -----------------------------------------------------------------------------
// xDrip /pebble endpoint
// -----------------------------------------------------------------------------
// xDrip uses /pebble for a simplified status/current reading response
// This mimics the format with "status" containing "now" timestamp and
// "bgs" containing the current reading
if ($path == '/pebble') {
    header('Content-Type: application/json');
    
    // Check authorization: look for HTTP_API_SECRET header
    // PHP converts "api-secret" header to "HTTP_API_SECRET" in $_SERVER
    $expectedHash = sha1('test22');
    $providedSecret = isset($_SERVER['HTTP_API_SECRET']) ? $_SERVER['HTTP_API_SECRET'] : '';
    if ($providedSecret !== $expectedHash) {
        echo 'Authentication failed';
        exit;
    }
    
    $nowMs = (int) floor(microtime(true) * 1000);
    $readingMs = $nowMs - (5 * 60 * 1000); // 5 minutes ago
    
    $response = [
        'status' => [
            ['now' => $nowMs]
        ],
        'bgs' => [
            [
                'sgv' => '9,6', // European decimal format (9.6 mmol/L)
                'trend' => 4,
                'direction' => 'Flat',
                'datetime' => $readingMs,
                'filtered' => 0,
                'unfiltered' => -127,
                'noise' => 1,
                'bgdelta' => '-0.2',
                'battery' => '',
                'iob' => 'unknown'
            ]
        ]
    ];
    
    echo json_encode($response);
    exit;
}

// -----------------------------------------------------------------------------
// xDrip /sgv.json endpoint
// -----------------------------------------------------------------------------
// xDrip uses /sgv.json for retrieving glucose readings
// This returns an array similar to Nightscout's /api/v1/entries format
if ($path == '/sgv.json') {
    header('Content-Type: application/json');
    
    // Check authorization: look for HTTP_API_SECRET header
    $expectedHash = sha1('test22');
    $providedSecret = isset($_SERVER['HTTP_API_SECRET']) ? $_SERVER['HTTP_API_SECRET'] : '';
    if ($providedSecret !== $expectedHash) {
        echo 'Authentication failed';
        exit;
    }
    
    // Respect count parameter
    $count = 16;
    if (isset($_GET['count'])) {
        $count = intval($_GET['count']);
    }
    if ($count < 0) $count = 0;
    if ($count > 200) $count = 200;
    
    $entries = [];
    for ($i = 0; $i < $count; $i++) {
        $d = generateRecord();
        $entries[$i] = [
            '_id' => $d['identifier'],
            'device' => 'xDrip-DexcomG5',
            'date' => $d['timestamp_ms'],
            'dateString' => $d['iso_date'],
            'sgv' => random_int(60, 200),
            'delta' => floatval(random_int(1,4).'.009'),
            'direction' => 'Flat',
            'type' => 'sgv',
            'filtered' => 0,
            'unfiltered' => 0,
            'rssi' => 100,
            'noise' => 1,
            'sysTime' => $d['iso_date'],
            'utcOffset' => 60,
            'mills' => $d['timestamp_ms']
        ];
    }
    
    echo json_encode($entries);
    exit;
}

// -----------------------------------------------------------------------------
// xDrip /status.json endpoint
// -----------------------------------------------------------------------------
// xDrip uses /status.json for configuration including bgHigh/bgLow thresholds
if ($path == '/status.json') {
    header('Content-Type: application/json');
    
    // Check authorization: look for HTTP_API_SECRET header
    $expectedHash = sha1('test22');
    $providedSecret = isset($_SERVER['HTTP_API_SECRET']) ? $_SERVER['HTTP_API_SECRET'] : '';
    if ($providedSecret !== $expectedHash) {
        echo 'Authentication failed';
        exit;
    }
    
    $response = [
        'status' => 'ok',
        'bgHigh' => 260,
        'bgLow' => 55,
        'bgTargetTop' => 180,
        'bgTargetBottom' => 80
    ];
    
    echo json_encode($response);
    exit;
}

if ($path == '/api/v1/status.json'){
$d = generateRecord();
$str = <<<STATUS
{
    "status": "ok",
    "name": "nightscout",
    "version": "15.0.2",
    "serverTime": "{$d['iso_date']}",
    "serverTimeEpoch": {$d['timestamp_ms']},
    "apiEnabled": true,
    "careportalEnabled": true,
    "boluscalcEnabled": false,
    "settings": {
        "units": "mmol",
        "timeFormat": 24,
        "dayStart": 7,
        "dayEnd": 21,
        "nightMode": false,
        "editMode": true,
        "showRawbg": "never",
        "customTitle": "Nightscout",
        "theme": "colors",
        "alarmUrgentHigh": true,
        "alarmUrgentHighMins": [
            30,
            60,
            90,
            120
        ],
        "alarmHigh": true,
        "alarmHighMins": [
            30,
            60,
            90,
            120
        ],
        "alarmLow": true,
        "alarmLowMins": [
            15,
            30,
            45,
            60
        ],
        "alarmUrgentLow": true,
        "alarmUrgentLowMins": [
            15,
            30,
            45
        ],
        "alarmUrgentMins": [
            30,
            60,
            90,
            120
        ],
        "alarmWarnMins": [
            30,
            60,
            90,
            120
        ],
        "alarmTimeagoWarn": true,
        "alarmTimeagoWarnMins": 15,
        "alarmTimeagoUrgent": true,
        "alarmTimeagoUrgentMins": 30,
        "alarmPumpBatteryLow": false,
        "language": "sv",
        "scaleY": "linear",
        "showPlugins": "dbsize delta direction upbat",
        "showForecast": "ar2",
        "focusHours": 3,
        "heartbeat": 60,
        "baseURL": "http://127.0.0.1",
        "authDefaultRoles": "denied",
        "thresholds": {
            "bgHigh": 260,
            "bgTargetTop": 180,
            "bgTargetBottom": 80,
            "bgLow": 55
        },
        "insecureUseHttp": true,
        "secureHstsHeader": true,
        "secureHstsHeaderIncludeSubdomains": false,
        "secureHstsHeaderPreload": false,
        "secureCsp": false,
        "deNormalizeDates": false,
        "showClockDelta": false,
        "showClockLastTime": false,
        "frameUrl1": "",
        "frameUrl2": "",
        "frameUrl3": "",
        "frameUrl4": "",
        "frameUrl5": "",
        "frameUrl6": "",
        "frameUrl7": "",
        "frameUrl8": "",
        "frameName1": "",
        "frameName2": "",
        "frameName3": "",
        "frameName4": "",
        "frameName5": "",
        "frameName6": "",
        "frameName7": "",
        "frameName8": "",
        "authFailDelay": 5000,
        "adminNotifiesEnabled": true,
        "authenticationPromptOnLoad": false,
        "DEFAULT_FEATURES": [
            "bgnow",
            "delta",
            "direction",
            "timeago"
        ],
        "alarmTypes": [
            "predict"
        ],
        "enable": [
            "careportal",
            "maker",
            "bolus",
            "bridge"
        ]
    },
    "extendedSettings": {
        "devicestatus": {
            "advanced": true,
            "days": 1
        }
    },
    "authorized": null,
    "runtimeState": "loaded"
}
STATUS;

} else {
$entries = [];

// Respect Nightscout's common `count` parameter.
// Default to 16 entries to match previous behavior.
$count = 16;
if (isset($_GET['count'])) {
        $count = intval($_GET['count']);
}
// Clamp to a reasonable range for tests.
if ($count < 0) $count = 0;
if ($count > 200) $count = 200;

for ($i = 0; $i < $count; $i++){
  $d = generateRecord();
  $entries[$i] = [
        "_id" => $d["identifier"],
        "device" => "xDrip-DexcomG5",
        "date" => $d["timestamp_ms"],
        "dateString" => $d["iso_date"],
        "sgv" => random_int(60, 200),
        "delta" => floatval(random_int(1,4).".009"),
        "direction" => "Flat",
        "type" => "sgv",
        "filtered" => 0,
        "unfiltered" => 0,
        "rssi" => 100,
        "noise" => 1,
        "sysTime" => $d["iso_date"],
        "utcOffset" => 60,
        "mills" => $d["timestamp_ms"]
    ];
}

}

// Very small and permissive authorization check used by the tests for Nightscout endpoints
$expectedHash = sha1('test22');
$providedSecret = isset($_SERVER['HTTP_API_SECRET']) ? $_SERVER['HTTP_API_SECRET'] : '';
if ($providedSecret === $expectedHash)
    echo $str === '' ? json_encode($entries) : $str;
else
  echo "Unauthorized";

