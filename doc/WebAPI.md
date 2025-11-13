# Trndi Web API Documentation

## Overview

Trndi includes an embedded HTTP API server that exposes glucose readings and predictions via REST endpoints. The server runs in a separate thread and does not interfere with the GUI's responsiveness.
> This is especially useful for Dexcom users, as they have no easy API access

## Configuration

#### Linux
Enable the web server in your configuration file (eg `~/.config/Trndi.cfg`):

```ini
[webserver]
enable=true
port=8080
token=your_optional_auth_token_here
```
#### Windows
In the Registry, access ```HKEY_CURRENT_USER\Software\Trndi```
Add these keys:
```ini
webserver.enable=true
webserver.port=8080
webserver.token=your_optional_auth_token_here
```

#### macOS
In your app's config add the values seen under Windows

### Configuration Options

- **enable**: Set to `true` to start the web server
- **port**: Port number for the HTTP server (default: 8080)
- **token**: Optional authentication token (leave empty for no authentication)

## Authentication

If a token is configured, requests must include it in the `Authorization` header:

```bash
curl -H "Authorization: Bearer your_token_here" http://localhost:8080/glucose
```

If no token is configured, all requests are allowed.

## Endpoints

### GET /glucose

Returns the current glucose reading with both mg/dL and mmol/L values.

**Response Format:**
```json
{
  "current": {
    "mgdl": "163.0",
    "mmol": "9.0",
    "mgdl_delta": "-5.0",
    "mmol_delta": "-0.3",
    "trend": 3,
    "timestamp": "2025-11-13 14:30:00"
  }
}
```

**Fields:**
- `mgdl`: Current glucose value in mg/dL
- `mmol`: Current glucose value in mmol/L (converted)
- `mgdl_delta`: Change since last reading in mg/dL
- `mmol_delta`: Change since last reading in mmol/L
- `trend`: Trend arrow (see Trend Values below)
- `timestamp`: Reading timestamp

**Status Codes:**
- `200 OK`: Data available
- `503 Service Unavailable`: No data available
- `500 Internal Server Error`: Service not configured

**Example:**
```bash
curl -s http://localhost:8080/glucose | jq '.current | {mgdl, mmol, trend}'
```

Output:
```json
{
  "mgdl": "87.0",
  "mmol": "4.8",
  "trend": 3
}
```

### GET /predict

Returns predicted glucose readings (if predictions are enabled).

**Response Format:**
```json
{
  "predictions": [
    {
      "mgdl": "157.4",
      "mmol": "8.7",
      "mgdl_delta": "-5.6",
      "mmol_delta": "-0.3",
      "trend": 7,
      "timestamp": "2025-11-13 14:35:00"
    },
    {
      "mgdl": "152.0",
      "mmol": "8.4",
      "mgdl_delta": "-5.4",
      "mmol_delta": "-0.3",
      "trend": 7,
      "timestamp": "2025-11-13 14:40:00"
    }
  ]
}
```

**Note:** Returns an empty array if predictions are not enabled or unavailable.

**Example:**
```bash
curl -s http://localhost:8080/predict | jq '.predictions[0] | {mgdl, mmol}'
```

### GET /status

Returns server status and data availability.

**Response Format:**
```json
{
  "status": "ok",
  "data_available": true
}
```

**Fields:**
- `status`: Always "ok" if server is running
- `data_available`: Boolean indicating if glucose data callbacks are configured

## Trend Values

The `trend` field uses the following numeric values:

| Value | Meaning | Arrow |
|-------|---------|-------|
| 0 | None | - |
| 1 | DoubleUp | ⇈ |
| 2 | SingleUp | ↑ |
| 3 | FortyFiveUp | ↗ |
| 4 | Flat | → |
| 5 | FortyFiveDown | ↘ |
| 6 | SingleDown | ↓ |
| 7 | DoubleDown | ⇊ |
| 8 | NotComputable | ? |
| 9 | RateOutOfRange | ⚠ |

## CORS Support

The API includes full CORS support with the following headers:

```
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET, POST, OPTIONS
Access-Control-Allow-Headers: Content-Type, Authorization
```

All endpoints support `OPTIONS` preflight requests.

## Technical Implementation

### Architecture

The web server is implemented using:
- **TThread**: Runs in a separate thread to avoid blocking the GUI
- **Raw BSD Sockets**: Uses `fpSocket`, `fpBind`, `fpListen`, `fpAccept` for direct socket control
- **Callback Pattern**: Accesses glucose data through function pointers to avoid direct coupling

### Thread Safety

The server uses a simple callback pattern where the main GUI thread maintains cached glucose readings that the web server thread reads. Since the web server only reads cached data and doesn't make API calls, no mutex or critical section is required.

**Callback Functions:**
```pascal
type
  TGetCurrentReadingFunc = function: BGReading of object;
  TGetPredictionsFunc = function: BGResults of object;
```

These callbacks are called from the web server thread and must:
1. Return quickly (no blocking operations)
2. Access only cached/pre-fetched data
3. Handle empty/missing data gracefully

### Startup Sequence

1. Web server configuration is read from `Trndi.cfg`
2. If `webserver.enable=true`, the `StartWebServer` function is called
3. A `TTrndiWebServer` instance is created with callbacks
4. The server thread starts and binds to the configured port
5. The thread enters an accept loop, handling one request at a time

### Shutdown

When the application closes:
1. `StopWebServer` is called in `FormDestroy`
2. The thread is terminated with `Terminate`
3. `WaitFor` ensures the thread completes
4. The socket is closed and resources are freed

## Integration Example

### JavaScript Client

```javascript
async function getCurrentGlucose() {
  try {
    const response = await fetch('http://localhost:8080/glucose');
    const data = await response.json();
    
    if (data.current) {
      console.log(`Glucose: ${data.current.mmol} mmol/L`);
      console.log(`Trend: ${data.current.trend}`);
    }
  } catch (error) {
    console.error('Failed to fetch glucose data:', error);
  }
}

setInterval(getCurrentGlucose, 60000); // Update every minute
```

### Python Client

```python
import requests
import time

def get_glucose():
    try:
        response = requests.get('http://localhost:8080/glucose')
        data = response.json()
        
        if 'current' in data:
            current = data['current']
            print(f"Glucose: {current['mmol']} mmol/L")
            print(f"Delta: {current['mmol_delta']} mmol/L")
            print(f"Trend: {current['trend']}")
    except Exception as e:
        print(f"Error: {e}")

while True:
    get_glucose()
    time.sleep(60)  # Poll every minute
```

### Home Assistant Integration

```yaml
# configuration.yaml
sensor:
  - platform: rest
    name: "Trndi Glucose"
    resource: "http://localhost:8080/glucose"
    value_template: "{{ value_json.current.mmol }}"
    unit_of_measurement: "mmol/L"
    json_attributes:
      - current
    scan_interval: 60

template:
  - sensor:
      - name: "Trndi Glucose Trend"
        state: >
          {% set trend = state_attr('sensor.trndi_glucose', 'current').trend %}
          {% set arrows = {
            1: '⇈', 2: '↑', 3: '↗', 4: '→',
            5: '↘', 6: '↓', 7: '⇊'
          } %}
          {{ arrows.get(trend, '?') }}
```

## Security Considerations

### Local Network Only

The web server binds to all interfaces (`INADDR_ANY`). To restrict to localhost only, you would need to modify the bind address in the code.

### Authentication Token

Store the authentication token securely:
- Use a strong random token (e.g., 32+ characters)
- Don't commit tokens to version control
- Rotate tokens periodically

### Firewall

Consider using a firewall to restrict access:

```bash
# Allow only from localhost
sudo ufw allow from 127.0.0.1 to any port 8080

# Allow from local network
sudo ufw allow from 192.168.1.0/24 to any port 8080
```

## Troubleshooting

### Server Won't Start

**Problem:** Web server doesn't respond to requests

**Solutions:**
1. Check configuration file: `cat ~/.config/Trndi.cfg | grep webserver`
2. Verify port is not in use: `sudo lsof -i :8080`
3. Check logs/terminal output for errors
4. Try a different port in configuration

### Connection Refused

**Problem:** `curl: (7) Failed to connect to localhost port 8080: Connection refused`

**Solutions:**
1. Verify Trndi is running
2. Confirm web server is enabled in config
3. Check firewall settings
4. Try connecting from different machine to test network access

### No Data Available (503 Error)

**Problem:** Server responds but returns 503 status

**Solutions:**
1. Wait for first glucose reading to arrive (can take 1-5 minutes)
2. Verify API backend is configured correctly
3. Check main GUI shows current reading
4. Review API credentials in settings

### Authentication Errors (401)

**Problem:** Requests fail with "Unauthorized"

**Solutions:**
1. Check token in config matches request header
2. Verify `Authorization: Bearer <token>` header format
3. Try without token (remove from config) to test

## Performance

### Resource Usage

- **Memory**: ~1-2 MB for web server thread
- **CPU**: Minimal (only active during requests)
- **Network**: Each request: ~500 bytes request + ~500-1000 bytes response

### Latency

Typical response times on localhost:
- `/glucose`: < 1ms
- `/predict`: < 5ms (depending on prediction count)
- `/status`: < 1ms

### Concurrent Requests

The current implementation handles requests sequentially (one at a time). This is sufficient for typical use cases (polling every 30-60 seconds). If you need high concurrency, consider:

1. Using a reverse proxy (nginx, Apache)
2. Implementing connection pooling
3. Caching responses with short TTL

## Advanced Configuration

### Custom Port

To use a port other than 8080:

```ini
[webserver]
port=3000
```

Remember to update firewall rules and client code accordingly.

### Disable Authentication

For trusted local network environments:

```ini
[webserver]
enable=true
port=8080
# token= (leave empty or omit)
```

### Integration with Reverse Proxy

Example nginx configuration:

```nginx
upstream trndi {
    server localhost:8080;
}

server {
    listen 443 ssl;
    server_name glucose.example.com;
    
    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;
    
    location / {
        proxy_pass http://trndi;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        
        # Add authentication at proxy level
        auth_basic "Trndi API";
        auth_basic_user_file /etc/nginx/.htpasswd;
    }
}
```

## Source Code

The web server implementation can be found in:
- `units/trndi/trndi.webserver.threaded.pp` - Main web server class
- `inc/umain_init.inc` - Startup/shutdown and callback implementations
- `units/forms/umain.pp` - Callback method declarations

## License

The web API is part of Trndi and follows the same GNU GPL v3 license. See LICENSE.md for details.
