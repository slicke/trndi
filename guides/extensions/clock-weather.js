// Show the current weather instead of the clock
const WEATHER_API_KEY = '<KEY>';

const getStockholmTemperature = () => {
    const q = encodeURIComponent('Stockholm');
    const url = `https://api.weatherapi.com/v1/current.json?key=${WEATHER_API_KEY}&q=${q}&aqi=no`;
    
    return jsonGet(url, "current.temp_c")
        .then(val => {
            return val;
        })
        .catch(e => {
            console.error('Failed to fetch Stockholm temperature', e);
            return 'N/A';
        });
};

let WeatherVal = '';
getStockholmTemperature().then(temp => { WeatherVal = temp.substring(0, 3); });

function clockView(glucose, time) {
    // You could trigger getStockholmTemperature() here if you want to update more often
    return WeatherVal;
}
