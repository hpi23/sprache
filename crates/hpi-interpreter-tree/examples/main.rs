use std::{collections::HashMap, env, fs, io, process, str::FromStr, time::Instant};

use hpi_interpreter_tree::{HPIHttpClient, RunError};
use reqwest::{
    header::{HeaderMap, HeaderName, HeaderValue},
    Method,
};

struct Client {}

impl HPIHttpClient for Client {
    fn request(
        &self,
        method: String,
        url: &str,
        body: String,
        headers: HashMap<String, String>,
    ) -> Result<(u16, String), String> {
        let client = reqwest::blocking::Client::builder()
            .build()
            .map_err(|err| err.to_string())?;

        let mut header_map = HeaderMap::new();
        for (key, value) in headers {
            header_map.insert(
                HeaderName::from_str(&key).map_err(|err| err.to_string())?,
                HeaderValue::from_str(&value).map_err(|err| err.to_string())?,
            );
        }

        let res = client
            .request(
                Method::from_str(&method).map_err(|err| err.to_string())?,
                url,
            )
            .body(body)
            .headers(header_map)
            .send()
            .map_err(|err| err.to_string())?;
        Ok((
            res.status().as_u16(),
            res.text().map_err(|err| err.to_string())?,
        ))
    }
}

fn main() {
    let start = Instant::now();
    let path = env::args().nth(1).unwrap();
    let code = fs::read_to_string(&path).unwrap();
    let client = Client {};
    match hpi_interpreter_tree::run(&code, &path, io::stdout(), client) {
        Ok((code, _)) => {
            println!("Program exited with code {code}");
            println!("{:?}", start.elapsed());
            process::exit(code as i32);
        }
        Err(RunError::Runtime(err)) => {
            eprintln!("{err}");
            process::exit(1);
        }
        Err(RunError::Analyzer(diagnostics)) => {
            for d in diagnostics {
                eprintln!("{d:#}");
            }
            process::exit(1);
        }
    }
}
