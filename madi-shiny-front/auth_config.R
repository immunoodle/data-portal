# --- File: auth_config.R ---

# Load required libraries
if (!require(httr2)) install.packages("httr2")
if (!require(jsonlite)) install.packages("jsonlite")

# Helper function for handling NULL or empty values
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Configuration (Load from Environment Variables or Secure Source) ---
DEX_ISSUER <- Sys.getenv("DEX_ISSUER")
DEX_CLIENT_ID <- Sys.getenv("DEX_CLIENT_ID")
DEX_CLIENT_SECRET <- Sys.getenv("DEX_CLIENT_SECRET")
DEX_LOGOUT_ENDPOINT <- Sys.getenv("DEX_LOGOUT_ENDPOINT")

# === CRITICAL FIX: Redirect URI Configuration ===
# Determine the correct redirect URI based on environment
determine_redirect_uri <- function() {
  # Check if HOSTNAME environment variable exists and is not empty
  hostname_env <- Sys.getenv("HOSTNAME")
  if (hostname_env != "") {
    return(hostname_env)
  }
  
  # Check if we're running in Docker (common environment variable)
  if (file.exists("/.docker.env")) {
    # Running in Docker - use localhost:3838
    return("http://localhost:3838")
  } else {
    # Running locally - use 127.0.0.1:3838
    return("http://127.0.0.1:3838")
  }
}

APP_REDIRECT_URI <- determine_redirect_uri()

# --- Fetch OIDC Discovery Information ---
get_oidc_discovery <- function(issuer_url) {
  discovery_url <- paste0(issuer_url, "/.well-known/openid-configuration")
  tryCatch({
    resp <- httr2::request(discovery_url) |> # Use httr2:: explicitly if not loading here
      httr2::req_perform() |>
      httr2::resp_body_json()
    return(resp)
  }, error = function(e) {
    warning(paste("Failed to fetch OIDC discovery document from:", discovery_url, "\nError:", e$message))
    return(NULL)
  })
}

oidc_config <- get_oidc_discovery(DEX_ISSUER)

# Extract endpoints
DEX_AUTH_ENDPOINT <- oidc_config$authorization_endpoint %||% DEX_AUTH_ENDPOINT
DEX_TOKEN_ENDPOINT <- oidc_config$token_endpoint %||% DEX_TOKEN_ENDPOINT
DEX_JWKS_ENDPOINT <- oidc_config$jwks_uri %||% DEX_JWKS_ENDPOINT
DEX_USERINFO_ENDPOINT <- oidc_config$userinfo_endpoint
DEX_LOGOUT_ENDPOINT <- oidc_config$end_session_endpoint %||% DEX_LOGOUT_ENDPOINT

# --- Create httr2 OAuth Client ---
dex_client <- httr2::oauth_client( # Use httr2:: explicitly if not loading here
  id = DEX_CLIENT_ID,
  secret = DEX_CLIENT_SECRET,
  token_url = DEX_TOKEN_ENDPOINT,
  name = "MADILumiReaderLocalClient"
)

# --- Define Scopes ---
OIDC_SCOPES <- "openid profile email"

# --- JWKS Cache ---
jwks_cache <- NULL
jwks_cache_time <- NULL
JWKS_CACHE_EXPIRY_SECS <- 5

# Function to fetch JWKS (JSON Web Key Set) for token validation
get_jwks <- function(jwks_url) {
  tryCatch({
    response <- httr2::request(jwks_url) |>
      httr2::req_timeout(10) |>
      httr2::req_perform()
    
    jwks_data <- httr2::resp_body_json(response)
    return(jwks_data$keys)
  }, error = function(e) {
    warning(paste("Failed to fetch JWKS from", jwks_url, ":", e$message))
    return(NULL)
  })
}

# Test OIDC discovery document (optional but helpful for debugging)
test_oidc_discovery <- function() {
  discovery_url <- paste0(DEX_ISSUER, "/.well-known/openid-configuration")
  
  tryCatch({
    response <- httr2::request(discovery_url) |>
      httr2::req_timeout(10) |>
      httr2::req_perform()
    
    discovery_data <- httr2::resp_body_json(response)
    
    cat("✅ OIDC Discovery successful\n")
    cat("Authorization endpoint:", discovery_data$authorization_endpoint %||% "Not found", "\n")
    cat("Token endpoint:", discovery_data$token_endpoint %||% "Not found", "\n")
    cat("JWKS URI:", discovery_data$jwks_uri %||% "Not found", "\n")
    
    return(TRUE)
  }, error = function(e) {
    warning(paste("Failed to fetch OIDC discovery document from:", discovery_url, "\nError:", e$message))
    return(FALSE)
  })
}

# Test the discovery endpoint
cat("Testing OIDC discovery...\n")
discovery_success <- test_oidc_discovery()

if (discovery_success) {
  cat("✅ Authentication configuration loaded successfully.\n")
} else {
  cat("⚠️ Authentication configuration loaded with warnings.\n")
}

# Print configuration for debugging
cat("=== OIDC Configuration ===\n")
cat("DEX_ISSUER:", DEX_ISSUER, "\n")
cat("DEX_CLIENT_ID:", DEX_CLIENT_ID, "\n")
cat("APP_REDIRECT_URI:", APP_REDIRECT_URI, "\n")
cat("OIDC_SCOPES:", OIDC_SCOPES, "\n")
cat("===========================\n")

cat("Authentication configuration loaded.\n")
