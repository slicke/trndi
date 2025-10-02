const WEATHER_API_KEY = 'be572a474bcc47ef99b183712250210';

async function getStockholmTemperature() {
    const url = `https://api.weatherapi.com/v1/current.json?key=${WEATHER_API_KEY}&q=Stockholm,Sweden&aqi=no`;
    try {
        const res = await asyncGet(url);

        console.log(res);
        // If asyncGet returns a Response-like object, check ok and parse JSON.
        if (res && typeof res.ok === 'boolean' && !res.ok) {
            throw new Error(`Weather API request failed: ${res.status} ${res.statusText}`);
        }

        let data;
        if (res && typeof res.json === 'function') {
            data = await res.json();
        } else {
            // Assume asyncGet already returned parsed JSON or other body.
            data = res;
        }

        const tempC = data && data.current && typeof data.current.temp_c === 'number' ? data.current.temp_c : null;
        if (tempC === null) throw new Error('Could not read temperature from response');
        return tempC;
    } catch (err) {
        console.error('Error fetching Stockholm temperature:', err);
        throw err;
    }
}

let WeatherVal = 0;
// Example usage: fetch and log the current temperature
getStockholmTemperature()
    .then(temp => WeatherVal = temp)
    .catch(() => {});

    Trndi.clockView = (glucose, time) => {
        return WeatherVal;
    }