<?php
// Fake NightScout for testing

function generateRecord() {
    // 1. Generera unik identifierare (liknande MongoDB ObjectId)
    $identifier = generateObjectId();

    // 2. Hämta aktuell Unix-tidsstämpel i millisekunder
    $timestampMs = round(microtime(true) * 1000);

    // 3. Konvertera tidsstämpeln till ISO 8601-format
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
    // 4 bytes för tidsstämpeln (sekunder sedan epoch)
    $time = dechex(time());
    $time = str_pad($time, 8, '0', STR_PAD_LEFT);

    // 5 bytes slumpmässiga bytes
    $random = bin2hex(random_bytes(5));

    // 3 bytes räknare (initieras slumpmässigt)
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
    // Om timestampen är som en sträng (för stora tal på 32-bitars system)
    if (is_string($milliseconds)) {
        $seconds = bcdiv($milliseconds, '1000', 0);
        $millis = bcmod($milliseconds, '1000');
    } else {
        $seconds = floor($milliseconds / 1000);
        $millis = $milliseconds % 1000;
    }

    // Skapa DateTime-objekt från sekunder
    $date = new DateTime("@$seconds");
    $date->setTimezone(new DateTimeZone('UTC')); // Ställ in tidszon till UTC

    // Lägg till millisekunder
    // PHP DateTime stöder endast mikrosekunder, så vi justerar därefter
    $date->modify("+{$millis} milliseconds");

    // Formatera till ISO 8601 med millisekunder
    // 'v' ger millisekunder i DateTime => =>format
    return $date->format('Y-m-d\TH:i:s.v\Z');
}

$entries = [];
$str = "";
if ($_SERVER['REQUEST_URI'] == '/api/v1/status.json'){
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
for ($i = 0; $i <= 15; $i++){
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

if (in_array(sha1('test22'), $_SERVER))
   echo $str === '' ? json_encode($entries) : $str;
else
  echo "Unauthorized";
