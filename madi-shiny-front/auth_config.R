# --- File: auth_config.R ---

# Load required libraries
if (!require(httr2)) install.packages("httr2")
if (!require(jsonlite)) install.packages("jsonlite")

# Helper function for handling NULL or empty values
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Configuration (Load from Environment Variables or Secure Source) ---
# Check if we're in local dev mode
IS_LOCAL_DEV <- Sys.getenv("LOCAL_DEV", unset = "0") == "1"

DEX_ISSUER <- Sys.getenv("DEX_ISSUER")
DEX_CLIENT_ID <- Sys.getenv("DEX_CLIENT_ID")
DEX_CLIENT_SECRET <- Sys.getenv("DEX_CLIENT_SECRET")
DEX_LOGOUT_ENDPOINT <- Sys.getenv("DEX_LOGOUT_ENDPOINT")
DEX_CA_CERT_PATH <- Sys.getenv("DEX_CA_CERT_PATH", unset = "")

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
get_oidc_discovery <- function(issuer_url, ca_cert_path = NULL) {
  discovery_url <- paste0(issuer_url, "/.well-known/openid-configuration")
  
  cat("🔍 [SSL DEBUG] get_oidc_discovery() called\n")
  cat("🔍 [SSL DEBUG] issuer_url:", issuer_url, "\n")
  cat("🔍 [SSL DEBUG] ca_cert_path provided:", !is.null(ca_cert_path) && nzchar(ca_cert_path), "\n")
  
  tryCatch({
    req <- httr2::request(discovery_url)
    
    # Add custom CA certificate if provided
    if (!is.null(ca_cert_path) && nzchar(ca_cert_path) && file.exists(ca_cert_path)) {
      cat("✅ [SSL DEBUG] Using custom CA certificate:", ca_cert_path, "\n")
      req <- req |> httr2::req_options(cainfo = ca_cert_path)
    } else if (!is.null(ca_cert_path) && nzchar(ca_cert_path)) {
      cat("⚠️  [SSL DEBUG] CA cert path provided but file not found:", ca_cert_path, "\n")
    } else {
      cat("ℹ️  [SSL DEBUG] No custom CA cert - using system defaults\n")
    }
    
    resp <- req |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    
    cat("✅ [SSL DEBUG] OIDC discovery successful!\n")
    return(resp)
    
  }, error = function(e) {
    warning(paste("Failed to fetch OIDC discovery document from:", discovery_url, "\nError:", e$message))
    return(NULL)
  })
}

# Skip OIDC discovery and configuration in local dev mode
if (IS_LOCAL_DEV) {
  cat("====================================\n")
  cat("⚠️  LOCAL_DEV mode enabled - Skipping OIDC configuration\n")
  cat("Authentication will be bypassed with dummy user credentials\n")
  cat("====================================\n")
  
  # Set dummy values to prevent errors
  oidc_config <- NULL
  DEX_AUTH_ENDPOINT <- NULL
  DEX_TOKEN_ENDPOINT <- NULL
  DEX_JWKS_ENDPOINT <- NULL
  DEX_USERINFO_ENDPOINT <- NULL
  dex_client <- NULL
  OIDC_SCOPES <- "openid profile email"
  
} else {
  cat("====================================\n")
  cat("🔐 Production OIDC mode enabled\n")
  cat("🔍 [SSL DEBUG] DEX_CA_CERT_PATH:", if(nzchar(DEX_CA_CERT_PATH)) DEX_CA_CERT_PATH else "(empty - using system CA bundle)", "\n")
  cat("====================================\n")
  
  # Production mode - perform normal OIDC discovery
  oidc_config <- get_oidc_discovery(DEX_ISSUER, DEX_CA_CERT_PATH)

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
}

# --- JWKS Cache ---
jwks_cache <- NULL
jwks_cache_time <- NULL
JWKS_CACHE_EXPIRY_SECS <- 5

# Function to fetch JWKS (JSON Web Key Set) for token validation
get_jwks <- function(jwks_url, ca_cert_path = NULL) {
  tryCatch({
    req <- httr2::request(jwks_url) |>
      httr2::req_timeout(10)
    
    # Add custom CA certificate if provided
    if (!is.null(ca_cert_path) && nzchar(ca_cert_path) && file.exists(ca_cert_path)) {
      req <- req |> httr2::req_options(cainfo = ca_cert_path)
    }
    
    response <- req |> httr2::req_perform()
    
    jwks_data <- httr2::resp_body_json(response)
    return(jwks_data$keys)
  }, error = function(e) {
    warning(paste("Failed to fetch JWKS from", jwks_url, ":", e$message))
    return(NULL)
  })
}

# Test OIDC discovery document (optional but helpful for debugging)
test_oidc_discovery <- function(ca_cert_path = NULL) {
  discovery_url <- paste0(DEX_ISSUER, "/.well-known/openid-configuration")
  
  tryCatch({
    req <- httr2::request(discovery_url) |>
      httr2::req_timeout(10)
    
    # Add custom CA certificate if provided
    if (!is.null(ca_cert_path) && nzchar(ca_cert_path) && file.exists(ca_cert_path)) {
      req <- req |> httr2::req_options(cainfo = ca_cert_path)
    }
    
    response <- req |> httr2::req_perform()
    
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

# Test the discovery endpoint (skip in dev mode)
if (!IS_LOCAL_DEV) {
  cat("Testing OIDC discovery...\n")
  discovery_success <- test_oidc_discovery(DEX_CA_CERT_PATH)

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
}

cat("Authentication configuration loaded.\n")
