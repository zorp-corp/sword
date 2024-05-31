/** Disk storage for events. */
use lmdb::{Environment, Error as LmdbError};
use std::fs;
use std::path::PathBuf;
use std::result::Result as StdResult;

#[derive(Debug, PartialEq)]
pub enum Error {
  InvalidPath,
  EpochNotFound,
  Lmdb(LmdbError),
}

pub type Result<T> = StdResult<T, Error>;

struct Disk {
  pub log_dir: PathBuf,
  pub epoch: u64, 
  pub env: Environment,
}

impl Disk {
  pub fn init(&self, log_dir: PathBuf) -> Result<Disk> {
    let epoch = self.last_epoch(&log_dir)?;
    let epoch_dir = log_dir.join(format!("0i{}", epoch));
    let env_builder = Environment::new();
    let env_res = env_builder.open(epoch_dir.as_path());
    match env_res {
      Ok(env) => {
        Ok(
          Disk {
            log_dir: log_dir,
            epoch: epoch,
            env: env,
          }
        )
      }
      Err(err) => Err(Error::Lmdb(err)),
    }
  }

  /// Get the number of the latest epoch in the given directory, or return 
  /// an error if there are no epochs or the path specified isn't a directory.
  pub fn last_epoch(&self, log_dir: &PathBuf) -> Result<u64> {
    if !log_dir.is_dir() {
      return Err(Error::InvalidPath);
    }

    let mut some = false;
    let mut last = 0;

    if let Ok(entries) = fs::read_dir(log_dir) {
      for entry in entries {
        if let Ok(entry) = entry {
          if let Some(name) = entry.file_name().to_str() {
            if let Some(epoch) = name.strip_prefix("0i") {
              if let Ok(n) = epoch.parse::<u64>() {
                some = true;
                if n > last {
                  last = n;
                }
              }
            }
          }
        }
      } 
    }

    if some {
      return Ok(last);
    }

    Err(Error::EpochNotFound)
  }
}