use dotenv::dotenv;
use reqwest::Client;
use serde::Deserialize;
use std::env;
use std::error::Error;
use std::fmt;
use chrono::{DateTime, Utc}; // Add this line to import DateTime and Utc

#[derive(Debug, Deserialize)]
#[serde(rename_all = "PascalCase")]
enum Direction {
    Flat,
    SingleUp,
    SingleDown,
    FortyFiveUp,
    FortyFiveDown,
    DoubleUp,
    DoubleDown,
    #[serde(other)]
    Unknown,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(unused)]
struct AuthResponse {
    token: String,
    sub: String,
    permission_groups: Vec<Vec<String>>,
    iat: u64,
    exp: u64,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(unused)]
struct Entry {
    device: String,

    // Use ISO 8601 date values directly as DateTime<Utc>
    date_string: DateTime<Utc>,
    sys_time: DateTime<Utc>, 

    utc_offset: i32,
    sgv: u32, // mgdl
    delta: f64, // diff in mgdl
    filtered: u32,
    unfiltered: u32,
    rssi: u32,
    noise: u32,

    direction: Direction, // @see the import for converting string -> Direction enum

    identifier: String,

    // type is a reserved keyword in Rust
    #[serde(rename = "type")]
    entry_type: String, 

    // u64 msec ts, set to deserialize from that to DateTime<Utc>
    #[serde(with = "chrono::serde::ts_milliseconds")]
    date: DateTime<Utc>,
    #[serde(with = "chrono::serde::ts_milliseconds")]
    srv_modified: DateTime<Utc>, 
    #[serde(with = "chrono::serde::ts_milliseconds")]
    srv_created: DateTime<Utc>, 
}

impl Entry {
    // Method to compute mmol from sgv
    pub fn mmol(&self) -> f64 {
        if self.sgv > 40 {
            return self.sgv as f64 / 18.0;
        }
        return self.sgv as f64;
    }

    pub fn mgdl(&self) -> u32 {
        if self.sgv > 40 {
            return self.sgv;
        }
        return self.sgv * 18 as u32;
    }

}

#[derive(Debug, Deserialize)]
struct EntriesResponse {
    status: u32,
    result: Vec<Entry>,
}

impl fmt::Display for Direction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Direction::Flat => write!(f, "Flat"),
            Direction::SingleUp => write!(f, "SingleUp"),
            Direction::SingleDown => write!(f, "SingleDown"),
            Direction::FortyFiveUp => write!(f, "FortyFiveUp"),
            Direction::FortyFiveDown => write!(f, "FortyFiveDown"),
            Direction::DoubleUp => write!(f, "DoubleUp"),
            Direction::DoubleDown => write!(f, "DoubleDown"),
            Direction::Unknown => write!(f, "Unknown"),
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load .env
    dotenv().ok();

    // Get the unique info
    let ns_token = env::var("NS_TOKEN").expect("NS_TOKEN is missing");
    let ns_url = env::var("NS_URL").expect("NS_URL is missing");

    let client = Client::new();

    // Get the Auth token
    match fetch_auth_token(&client, &ns_url, &ns_token).await {
        Ok(auth_response) => {
            // Required permissions
            let required_permissions = vec![
                "api:devicestatus:read",
                "api:entries:read",
                "api:profile:read",
                "api:treatments:read",
            ];

            // Verify that the the required permissions
            let has_permissions = auth_response.permission_groups.iter().any(|group| {
                required_permissions.iter().all(|perm| {
                    group.iter().any(|p| p == perm)
                })
            });

            // Check if the permissions are availables
            if has_permissions {
                fetch_entries(&client, &ns_url, &auth_response.token, None).await?;
            } else {
                eprintln!("Missing required API permissions");
            }
        }
        Err(e) => {
            eprintln!("Cannot fetch API token: {}", e);
        }
    }

    Ok(())
}

async fn fetch_auth_token(
    client: &Client,
    base_url: &str,
    token_suffix: &str,
) -> Result<AuthResponse, Box<dyn std::error::Error>> {
    // Bygg URL:en för att hämta token
    let auth_url = format!(
        "https://{}/api/v2/authorization/request/{}",
        base_url, token_suffix
    );

    // Hämta token
    let res = client.get(&auth_url).send().await?;
   
    if res.status().is_success() {
        let auth_response: AuthResponse = res.json().await?;
        print!("{}", auth_response.token);
        Ok(auth_response)
    } else {
        Err(format!("Statuskod: {}", res.status()).into())
    }
}

async fn fetch_entries(
    client: &Client,
    base_url: &str,
    token: &str,
    filter: Option<Vec<(&str, &str)>>
) -> Result<(), Box<dyn std::error::Error>> {
    let entries_url = format!("https://{}/api/v3/entries.json", base_url);

    let params = match filter {
        Some(f) => f,
        None => vec![
            ("limit", "20"),
            ("sort$desc", "date"),
        ],
    };

    let url = reqwest::Url::parse_with_params(&entries_url, &params)?;
    // Anropa URL:en med Authorization-header
    let res = client
        .get(url)
        .header("Authorization", format!("Bearer {}", token))
        .send()
        .await?;

    if res.status().is_success() {
       /* print!("{}", res.json().await?);
        return Err("Failed to fetch entries".into());*/
        // Deserialisera JSON-svaret till EntriesResponse
        let entries_response: EntriesResponse = res.json().await?;

        // Kontrollera statuskoden
        if entries_response.status == 200 {
            // Iterera över varje entry och gör något med dem
            for entry in entries_response.result {

                println!("Device: {}", entry.device);
                println!("Date: {}", entry.date_string);
                println!("SGV: {}", entry.sgv);
                println!("Reading (mmol/L): {}", entry.mmol());
                println!("Reading (mg/dL): {}", entry.mgdl());


                // Hantera direction med match
                match entry.direction {
                    Direction::Unknown => eprintln!("Varning: Okänd direction: {}", entry.direction),
                    _ => println!("Direction: {}", entry.direction),
                }

                println!("Type: {}", entry.entry_type);
                println!("---");
            }
        } else {
            eprintln!("API svarade med status: {}", entries_response.status);
        }
    } else {
        eprintln!("Misslyckades med att hämta entries: {}", res.status());
    }

    Ok(())
}