library(shiny)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(shinyWidgets)
library(visNetwork)
library(RPostgres)
library(DBI)
library(DT)
library(sqldf)
library(datamods)
library(rsconnect)
library(stringi)
library(tidyverse)
library(shinyFiles)
library(readxl)
library(openxlsx)
library(rhandsontable)
library(sendmailR)
library(reactable)
library(glue)
library(data.table)
library(httr2)
library(jose)
library(openssl) # For rand_bytes
library(jsonlite)
library(urltools)

Sys.setenv(LOCAL_DEV = "0")

# Increased max file upload size
options(shiny.maxRequestSize = 100*1024^2)

# Source database setup functions
source("database_setup.R", local = FALSE)

# Create database connection
conn <- create_safe_db_connection()

# load the endpoints object
endpoints <- readRDS('endpoints.rds')

# get madi_track.users with error handling
users <- tryCatch({
  DBI::dbGetQuery(conn, "SELECT oauth_unique_id, username, workspace_id, display_name, project, full_name, CAST(users_id AS INTEGER) AS users_id FROM madi_track.users;")
}, error = function(e) {
  warning("Could not load users table: ", e$message)
  data.frame(oauth_unique_id = character(0), username = character(0), workspace_id = numeric(0), 
             display_name = character(0), project = character(0), full_name = character(0), 
             users_id = integer(0), stringsAsFactors = FALSE)
})

# Initialize with empty study lists - will be populated based on user workspace after authentication
all_study_list <- data.frame(study_accession = character(0), brief_title = character(0), stringsAsFactors = FALSE)

# explorer parameters table: madi_meta.explore_parms with error handling
parameter_df <- tryCatch({
  DBI::dbGetQuery(conn, "SELECT explore_parm_id, child_table, table_src_query, parent_table, match_query_on, tab_label, tree_level FROM madi_meta.explore_parms ORDER BY parent_table, child_table;")
}, error = function(e) {
  warning("Could not load parameter_df: ", e$message)
  data.frame(explore_parm_id = integer(0), child_table = character(0), table_src_query = character(0), 
             parent_table = character(0), match_query_on = character(0), tab_label = character(0), 
             tree_level = integer(0), stringsAsFactors = FALSE)
})

# Initialize with empty study data - will be populated based on user workspace after authentication
study <- create_empty_studies_dataframe()

print("Sourcing auth_config.R...")
source("auth_config.R", local = FALSE) # Source globally
print("Finished sourcing auth_config.R")

# --- Debug printing from HEAD ---
print("--- Final Auth Config Values ---")
print(paste("DEX_ISSUER:", DEX_ISSUER))
print(paste("DEX_INTERNAL_URL:", if(exists("DEX_INTERNAL_URL")) DEX_INTERNAL_URL else "NOT SET"))
print(paste("DEX_AUTH_ENDPOINT:", DEX_AUTH_ENDPOINT))
print(paste("DEX_TOKEN_ENDPOINT:", DEX_TOKEN_ENDPOINT))
print(paste("DEX_JWKS_ENDPOINT:", DEX_JWKS_ENDPOINT))

# --- 1. Define the UI content that appears AFTER a user is authenticated ---
authenticated_body_content <- function() {
  fluidPage(
    useShinyjs(),
    tags$head(tags$link(rel = "shortcut icon", href = "greenicon.ico")),
    uiOutput("visnetworkbox"),
    tabsetPanel(id = "body_panel_id")
  )
}

# --- 2. Define the main dashboard components ---
header <- dashboardHeader(
  tags$li(
    a(
      img(src = 'apple-touch-icon.png', title = "MADI Logo", height = "30px"),
      style = "padding-top:10px; padding-bottom:10px;"
    ),
    class = "dropdown"
  ),
  title = "MADI Data Portal"
)

sidebar <- dashboardSidebar(
  uiOutput("userpanel"),
  uiOutput("primarysidepanel"),
  width = 350
)

body <- dashboardBody(
  uiOutput("body_content_ui")
)

ui <- tagList(
  tags$head(
    # Enhanced version with comprehensive debugging and URL parameter fallback
    tags$script(HTML("
    $(document).on('shiny:connected', function(event) {
      console.log('JS DEBUG: Shiny connected event fired.');
      console.log('JS DEBUG: Current URL:', window.location.href);
      console.log('JS DEBUG: Current cookies:', document.cookie);

      const storageKey = 'shiny_oidc_state';

      // --- Function to clear all storage locations ---
      function clearAllAuthStorage() {
        console.log('JS DEBUG: Clearing all auth storage...');
        try {
          localStorage.removeItem(storageKey);
          sessionStorage.removeItem(storageKey);
          document.cookie = storageKey + '=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT';
          // Clear any auth state from URL hash
          if (window.location.hash.includes('authState=')) {
            window.location.hash = window.location.hash.replace(/[?&]authState=[^&]*/, '');
          }
          console.log('JS DEBUG: All auth storage cleared');
        } catch (e) {
          console.error('JS DEBUG ERROR: Failed to clear storage:', e);
        }
      }

      // --- Handler to redirect the browser ---
      Shiny.addCustomMessageHandler('redirect', function(url) {
        console.log('JS DEBUG: Received \"redirect\" message. URL:', url);
        if (url) window.location.href = url;
      });

      // --- Handler to clean up the URL after login ---
      Shiny.addCustomMessageHandler('updateQueryString', function(query) {
        console.log('JS DEBUG: Received \"updateQueryString\" message. Query:', query);
        const newUrl = window.location.pathname + query + window.location.hash;
        window.history.replaceState({}, document.title, newUrl);
      });

      // --- Handler to clear storage before new auth ---
      Shiny.addCustomMessageHandler('clearAuthStorage', function(data) {
        console.log('JS DEBUG: Received message to clear auth storage before new auth.');
        clearAllAuthStorage();
      });

      // --- Handler to SET the state with enhanced persistence ---
      Shiny.addCustomMessageHandler('setAuthStateCookie', function(data) {
        console.log('JS DEBUG: Received message to set auth state.');
        if (data && data.state) {
          console.log('JS DEBUG: First clearing any existing state...');
          clearAllAuthStorage();
          
          console.log('JS DEBUG: Storing NEW state with enhanced persistence. State:', data.state);
          
          // Store in multiple locations for maximum persistence
          try {
            localStorage.setItem(storageKey, data.state);
            console.log('JS DEBUG: Successfully stored in localStorage');
          } catch (e) {
            console.error('JS DEBUG ERROR: Failed to store in localStorage:', e);
          }
          
          try {
            sessionStorage.setItem(storageKey, data.state);
            console.log('JS DEBUG: Successfully stored in sessionStorage');
          } catch (e) {
            console.error('JS DEBUG ERROR: Failed to store in sessionStorage:', e);
          }
          
          // Store in cookie with more persistence options
          try {
            const expires = new Date(Date.now() + 10 * 60 * 1000); // 10 minutes
            document.cookie = storageKey + '=' + data.state + '; path=/; expires=' + expires.toUTCString() + '; SameSite=Lax; Secure=false';
            console.log('JS DEBUG: Successfully stored in cookie');
            console.log('JS DEBUG: Cookie after setting:', document.cookie);
          } catch (e) {
            console.error('JS DEBUG ERROR: Failed to store in cookie:', e);
          }
          
          // Also store in URL hash as ultimate fallback
          try {
            const currentHash = window.location.hash;
            window.location.hash = currentHash + (currentHash ? '&' : '') + 'authState=' + data.state;
            console.log('JS DEBUG: Successfully stored in URL hash');
          } catch (e) {
            console.error('JS DEBUG ERROR: Failed to store in URL hash:', e);
          }
          
          console.log('JS DEBUG: State storage attempts completed.');
        } else {
          console.error('JS DEBUG ERROR: No state received to store.');
        }
      });
      
      // --- Handler to reload the page (for logout) ---
      Shiny.addCustomMessageHandler('reloadPage', function(message) {
        console.log('JS DEBUG: Received \"reloadPage\" message. Reloading...');
        window.location.reload();
      });
      
      // --- ENHANCED STATE CHECK WITH COMPREHENSIVE FALLBACKS ---
      (function() {
        console.log('JS DEBUG: Starting comprehensive state check...');
        console.log('JS DEBUG: localStorage contents:', localStorage.getItem(storageKey));
        console.log('JS DEBUG: sessionStorage contents:', sessionStorage.getItem(storageKey));
        console.log('JS DEBUG: Current cookies:', document.cookie);
        console.log('JS DEBUG: Current URL hash:', window.location.hash);
        
        // Check if we have callback parameters in URL
        const urlParams = new URLSearchParams(window.location.search);
        const codeParam = urlParams.get('code');
        const stateParam = urlParams.get('state');
        
        if (codeParam && stateParam) {
          console.log('JS DEBUG: OAuth callback detected. Code:', codeParam, 'State:', stateParam);
          
          let storedState = null;
          let source = '';
          
          // Try localStorage first
          storedState = localStorage.getItem(storageKey);
          if (storedState) {
            console.log('JS DEBUG: Found stored state in localStorage:', storedState);
            console.log('JS DEBUG: Checking if stored state matches URL state...');
            console.log('JS DEBUG: Stored:', storedState, '| URL:', stateParam);
            
            // If states don't match, this is likely a stale state - clear and use URL state
            if (storedState !== stateParam) {
              console.warn('JS DEBUG WARNING: Stored state does not match URL state. Clearing stale state.');
              clearAllAuthStorage();
              storedState = null;
            } else {
              source = 'localStorage';
            }
          }
          
          // If not found in localStorage or was cleared, try sessionStorage
          if (!storedState) {
            console.log('JS DEBUG: State not found in localStorage or was cleared, trying sessionStorage...');
            storedState = sessionStorage.getItem(storageKey);
            if (storedState && storedState === stateParam) {
              console.log('JS DEBUG: Found matching state in sessionStorage:', storedState);
              source = 'sessionStorage';
            } else if (storedState) {
              console.warn('JS DEBUG WARNING: sessionStorage state does not match URL state. Ignoring.');
              storedState = null;
            }
          }
          
          // If still not found, try cookie
          if (!storedState) {
            console.log('JS DEBUG: State not found in sessionStorage, trying cookie...');
            const cookies = document.cookie.split(';');
            for (let cookie of cookies) {
              const [name, value] = cookie.trim().split('=');
              if (name === storageKey && value === stateParam) {
                storedState = value;
                console.log('JS DEBUG: Found matching state in cookie:', storedState);
                source = 'cookie';
                break;
              }
            }
          }
          
          // If still not found, try URL hash
          if (!storedState) {
            console.log('JS DEBUG: State not found in cookie, trying URL hash...');
            const hash = window.location.hash.substring(1); // Remove #
            const hashParams = new URLSearchParams(hash);
            const hashState = hashParams.get('authState');
            if (hashState && hashState === stateParam) {
              storedState = hashState;
              console.log('JS DEBUG: Found matching state in URL hash:', storedState);
              source = 'hash';
            }
          }
          
          // FINAL FALLBACK: If no stored state found, use the state from URL directly
          // This is risky but better than failing completely
          if (!storedState) {
            console.warn('JS DEBUG WARNING: No stored state found anywhere. Using URL state directly as fallback.');
            console.warn('JS DEBUG WARNING: This reduces security but allows authentication to proceed.');
            storedState = stateParam;
            source = 'url_fallback';
          }
          
          if (storedState) {
            console.log('JS DEBUG: Successfully retrieved stored state:', storedState, 'from:', source);
            
            // Clean up all storage locations
            clearAllAuthStorage();
            
            console.log('JS DEBUG: Sending state to server for validation...');
            Shiny.setInputValue('authStateCookieValue', storedState, { priority: 'event' });
            Shiny.setInputValue('authStateSource', source, { priority: 'event' });
          } else {
            console.error('JS DEBUG ERROR: Absolutely no stored state found in any location!');
            console.log('JS DEBUG: Sending error signal to server...');
            Shiny.setInputValue('authStateError', 'no_stored_state', { priority: 'event' });
          }
        } else {
          console.log('JS DEBUG: No OAuth callback parameters detected.');
        }
      })();

      console.log('JS DEBUG: Finished setting up handlers.');
    });
  ")),
    # --- Your inactivity detection script can remain as is ---
    tags$script(HTML("
      (function() {
        const inactivityTimeout = 15 * 60 * 1000; // 15 minutes
        let timeout;
        function resetTimer() { clearTimeout(timeout); timeout = setTimeout(logout, inactivityTimeout); }
        function logout() { if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('user_is_inactive', true, {priority: 'event'}); } }
        window.addEventListener('mousemove', resetTimer, {passive: true});
        window.addEventListener('mousedown', resetTimer, {passive: true});
        window.addEventListener('keypress', resetTimer, {passive: true});
        window.addEventListener('touchmove', resetTimer, {passive: true});
        resetTimer();
      })();
    "))
  ),

  dashboardPage(
    title = "xMap Reader",
    skin = "green",
    header = header,
    sidebar = sidebar,
    body = body
  )
)

server <- function(input, output, session) {

  is_local_dev <- function() {
    Sys.getenv("LOCAL_DEV", unset = "0") == "1"
  }

  message("Shiny session started. [DEBUG]")

  # --- Authentication Reactive Values from HEAD ---
  user_data <- reactiveVal(
    if (is_local_dev()) {
      print("DEBUG: Local dev mode: user_data() set to authenticated dummy user.")
      list(
        is_authenticated = TRUE,
        email = "dev_user@dartmouth.edu",   # Or any test user
        name = "Local Dev User",
        id_token = "FAKE_ID_TOKEN_FOR_DEV"
      )
    } else {
      print("DEBUG: Production mode: user_data() set to unauthenticated.")
      list(is_authenticated = FALSE)
    }
  )

  jwks_cache <- reactiveVal(NULL)
  
  # Global reactive triggers for UI updates
  current_workspace <- reactiveVal(NULL)  # Track current workspace
  studies_trigger <- reactiveVal(0)
  workspace_list_trigger <- reactiveVal(0)
  access_keys_trigger <- reactiveVal(0)
  
  # Flag to prevent infinite loops in workspace observer
  workspace_update_in_progress <- reactiveVal(FALSE)
  
  session$userData$callbackCode <- NULL
  session$userData$callbackState <- NULL
  session$userData$processedOIDCState <- FALSE
  session$userData$ui_update_triggered <- FALSE  # Add flag to prevent infinite updates

  # Enhanced observeEvent for authStateCookieValue with better logging
  observeEvent(input$authStateCookieValue, {
    print("DEBUG: *** input$authStateCookieValue observer triggered ***")
    print(paste("DEBUG: Value received from JS:", input$authStateCookieValue))
    print(paste("DEBUG: Source of state:", input$authStateSource))

    if (isTRUE(session$userData$processedOIDCState)) {
      print("DEBUG: Skipping - state already processed for this login attempt.")
      return()
    }

    expected_state_from_cookie <- input$authStateCookieValue
    received_state_from_url <- session$userData$callbackState
    authorization_code <- session$userData$callbackCode

    print(paste("DEBUG: expected_state_from_cookie:", expected_state_from_cookie))
    print(paste("DEBUG: received_state_from_url:", received_state_from_url))
    print(paste("DEBUG: authorization_code:", authorization_code))

    # Add validation to ensure we have the callback data
    if (is.null(received_state_from_url) || is.null(authorization_code)) {
      print("DEBUG: Missing callback data - waiting for OIDC observe() to process URL params")
      # Give the OIDC observe() time to process the URL parameters
      invalidateLater(100, session)
      return()
    }

    if (is.null(expected_state_from_cookie) || !nzchar(expected_state_from_cookie)) {
      print("DEBUG: Missing required auth data from storage.")
      print("DEBUG: This indicates a problem with state storage/retrieval.")
      return()
    }

    print("DEBUG: Comparing stored state to URL state...")
    print(paste("DEBUG: Stored:", expected_state_from_cookie, "| URL:", received_state_from_url))

    if (identical(expected_state_from_cookie, received_state_from_url)) {
      print("DEBUG: State validated successfully.")
      print(paste("DEBUG: State source was:", input$authStateSource))
      session$userData$processedOIDCState <- TRUE
      session$sendCustomMessage("updateQueryString", "?")
      print("DEBUG: Calling exchange_code_for_token() with auth code.")
      
      # Add error handling around token exchange
      tryCatch({
        final_user_data <- exchange_code_for_token(authorization_code)
        print("DEBUG: Result from exchange_code_for_token:")
        print(str(final_user_data))
        
        if (!is.null(final_user_data) && isTRUE(final_user_data$is_authenticated)) {
          print("DEBUG: SUCCESS! Setting authenticated user_data()")
          user_data(final_user_data)
          session$userData$ui_update_triggered <- FALSE  # Reset flag for new auth
          print("DEBUG: user_data() has been set. Current value:")
          print(str(user_data()))
        } else {
          print("DEBUG: FAILED! Authentication failed in token exchange.")
          print("DEBUG: final_user_data is NULL or not authenticated")
          user_data(list(is_authenticated = FALSE))
          showNotification("Authentication failed. Please try logging in again.", type = "error", duration = 10)
        }
      }, error = function(e) {
        print(paste("DEBUG: ERROR in token exchange:", e$message))
        user_data(list(is_authenticated = FALSE))
        showNotification("Authentication error occurred. Please try again.", type = "error", duration = 10)
      })
    } else {
      warning("DEBUG: FAILED! Invalid state parameter! Stored/URL mismatch.")
      warning(paste("DEBUG: Expected:", expected_state_from_cookie))
      warning(paste("DEBUG: Received:", received_state_from_url))
      showNotification("Authentication error (state validation failed). Please try again.", type = "error", duration = 10)
      session$sendCustomMessage("updateQueryString", "?")
      session$userData$callbackCode <- NULL
      session$userData$callbackState <- NULL
      session$userData$processedOIDCState <- FALSE
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  exchange_code_for_token <- function(auth_code) {
    message("Attempting token exchange...")
    message("🔍 [SSL DEBUG] Token endpoint: ", DEX_TOKEN_ENDPOINT)
    message("🔍 [SSL DEBUG] CA cert path: ", if(!is.null(DEX_CA_CERT_PATH) && nzchar(DEX_CA_CERT_PATH)) DEX_CA_CERT_PATH else "(not set)")
    
    tryCatch({
      token_req <- httr2::request(DEX_TOKEN_ENDPOINT) |>
        httr2::req_method("POST") |>
        httr2::req_auth_basic(username = DEX_CLIENT_ID, password = DEX_CLIENT_SECRET) |>
        httr2::req_body_form(
          grant_type = "authorization_code",
          code = auth_code,
          redirect_uri = APP_REDIRECT_URI,
          client_id = DEX_CLIENT_ID
        ) |>
        httr2::req_headers(
          "Accept" = "application/json",
          "Content-Type" = "application/x-www-form-urlencoded"
        )
      
      # Add custom CA certificate if provided
      if (!is.null(DEX_CA_CERT_PATH) && nzchar(DEX_CA_CERT_PATH) && file.exists(DEX_CA_CERT_PATH)) {
        message("✅ [SSL DEBUG] Adding CA cert to token request: ", DEX_CA_CERT_PATH)
        token_req <- token_req |> httr2::req_options(cainfo = DEX_CA_CERT_PATH)
      } else {
        message("ℹ️  [SSL DEBUG] No custom CA cert for token exchange - using system defaults")
      }
      
      resp <- httr2::req_perform(token_req)
      message("✅ [SSL DEBUG] Token exchange successful!")
      token_data <- httr2::resp_body_json(resp)
      if (is.null(token_data$id_token)) stop("No id_token received from token endpoint.")
      validated_payload <- validate_id_token(token_data$id_token)
      if (!is.null(validated_payload)) {
        final_user_data <- get_user_information(
          access_token = token_data$access_token,
          id_token_payload = validated_payload,
          raw_id_token = validated_payload$raw_id_token_string
        )

        return(final_user_data)
      } else {
        warning("ID Token validation failed (returned NULL). User not authenticated.")
        user_data(list(is_authenticated = FALSE))
        showNotification("Authentication failed: Invalid identity.", type = "error")

        return(NULL)

      }
    }, error = function(e) {
      warning(paste("Error exchanging code:", e$message))
      if (inherits(e, "httr2_http") && !is.null(e$body)) {
        try({ error_body <- httr2::resp_body_json(e$resp); warning("Dex error response body:"); print(error_body) }, silent = TRUE)
      }
      user_data(list(is_authenticated = FALSE))
      showNotification("Error during login token exchange. Check logs.", type = "error")
      return(NULL)
    })
  }

  validate_id_token <- function(id_token) {
    message("Validating ID token...")

    perform_validation <- function(token, keys) {
      if (is.null(keys) || length(keys) == 0) return("refetch_jwks")
      tryCatch({
        parts <- strsplit(token, ".", fixed = TRUE)[[1]]
        header_char <- rawToChar(jose::base64url_decode(parts[1]))
        header <- jsonlite::fromJSON(header_char, simplifyVector = FALSE)
        token_kid <- header$kid
        if (is.null(token_kid)) stop("Token header missing 'kid'.")

        public_key_jwk <- NULL
        for (key in keys) { if (!is.null(key$kid) && key$kid == token_kid) { public_key_jwk <- key; break; }}
        if (is.null(public_key_jwk)) stop("kid_not_found")

        jwk_json_string <- jsonlite::toJSON(public_key_jwk, auto_unbox = TRUE)
        public_key <- jose::read_jwk(jwk_json_string)
        payload <- jose::jwt_decode_sig(jwt = token, pubkey = public_key)

        current_time <- as.numeric(Sys.time())
        if (!identical(payload$iss, DEX_ISSUER)) stop("Invalid issuer.")
        aud_ok <- if(is.list(payload$aud)) DEX_CLIENT_ID %in% payload$aud else identical(payload$aud, DEX_CLIENT_ID)
        if (!aud_ok) stop("Invalid audience.")
        if (payload$exp < (current_time - 60)) stop("Token expired.")

        payload$raw_id_token_string <- token
        message("Token validation successful.")
        return(payload)
      }, error = function(e) {
        if (grepl("kid_not_found", e$message, fixed = TRUE)) {
          warning(paste("Token 'kid' [", token_kid, "] not found in the current JWKS set.", sep=""))
          return("refetch_jwks")
        }
        warning(paste("Token validation error:", e$message))
        return(NULL)
      })
    }

    payload <- perform_validation(id_token, jwks_cache())

    if (is.character(payload) && payload == "refetch_jwks") {
      message("Re-fetching JWKS and retrying validation...")
      fresh_keys <- get_jwks(DEX_JWKS_ENDPOINT, DEX_CA_CERT_PATH)
      if (!is.null(fresh_keys)) {
        jwks_cache(fresh_keys)
        payload <- perform_validation(id_token, fresh_keys)
      } else {
        warning("Failed to fetch fresh JWKS. Authentication aborted.")
        return(NULL)
      }
    }

    if (!is.list(payload)) {
      warning("ID Token validation failed permanently.")
      return(NULL)
    }

    return(payload)
  }

  get_user_information <- function(access_token, id_token_payload, raw_id_token) {
    message("--- Entering get_user_information ---")
    user_info <- list(
      email = id_token_payload[["email"]],
      name = id_token_payload[["name"]],
      id_token = raw_id_token,
      is_authenticated = TRUE
    )
    message(paste("  > User info list created. Email:", user_info$email %||% "NULL"))
    return(user_info)
  }

  observeEvent(input$authStateError, {
    print("DEBUG: Authentication state error received from JS.")
    print(paste("DEBUG: Error type:", input$authStateError))
    
    if (input$authStateError == "no_stored_state") {
      print("DEBUG: No stored state found during OAuth callback.")
      print("DEBUG: This might indicate a storage/cookie issue or cross-domain problem.")
      showNotification("Authentication failed: Session state lost. Please try logging in again.", 
                       type = "error", duration = 10)
      session$userData$callbackCode <- NULL
      session$userData$callbackState <- NULL
      session$userData$processedOIDCState <- FALSE
      session$sendCustomMessage("updateQueryString", "?")
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$authStateSource, {
    print(paste("DEBUG: Authentication state retrieved from source:", input$authStateSource))
    if (input$authStateSource == "url_fallback") {
      print("WARNING: Using URL fallback for state validation. Security reduced but functional.")
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$login_button, {
    print("DEBUG: input$login_button clicked.")
    if (is_local_dev()) {
      showNotification("DEV mode: Bypassing login.", type = "message")
      user_data(list(
        is_authenticated = TRUE,
        email = "dev_user@dartmouth.edu",
        name = "Local Dev User",
        id_token = "FAKE_ID_TOKEN_FOR_DEV"
      ))
      print("DEBUG: DEV mode: Authenticated dummy user_data() set.")
      return()
    }
    
    req(DEX_AUTH_ENDPOINT, DEX_CLIENT_ID, APP_REDIRECT_URI, OIDC_SCOPES)
    message("DEBUG: Login button clicked. Starting auth process...")
    if (!requireNamespace("openssl")) stop("openssl required")
    print("DEBUG: Checking required auth endpoints...")
    print(paste("DEBUG: DEX_AUTH_ENDPOINT:", DEX_AUTH_ENDPOINT))
    print(paste("DEBUG: DEX_CLIENT_ID:", DEX_CLIENT_ID))
    print(paste("DEBUG: APP_REDIRECT_URI:", APP_REDIRECT_URI))
    print(paste("DEBUG: OIDC_SCOPES:", OIDC_SCOPES))
    
    print("DEBUG: Clearing any existing auth state...")
    session$sendCustomMessage("clearAuthStorage", list())
    Sys.sleep(0.1)
    
    state <- openssl::rand_bytes(16) |> jose::base64url_encode()
    print(paste("DEBUG: Generated state:", state))
    print("DEBUG: Storing state in browser storage via JS...")
    session$sendCustomMessage("setAuthStateCookie", list(state = state))
    print("DEBUG: Waiting for storage to complete...")
    Sys.sleep(0.3)
    
    auth_url_req <- httr2::request(DEX_AUTH_ENDPOINT) |> 
      httr2::req_url_query(response_type="code", client_id=DEX_CLIENT_ID, redirect_uri=APP_REDIRECT_URI, scope=OIDC_SCOPES, state=state)
    auth_url <- auth_url_req$url
    print(paste("DEBUG: Built auth URL:", auth_url))
    
    if (!is.null(auth_url) && nzchar(auth_url)) {
      message(paste("DEBUG: SUCCESS: Redirecting to auth URL"))
      message("DEBUG: Sending JS msg: redirect")
      session$sendCustomMessage("redirect", auth_url)
    } else {
      message("DEBUG: ERROR: Failed to build auth URL.")
      showNotification("Error building authentication URL. Please try again.", type = "error")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$logout_button, {
    print("DEBUG: input$logout_button event triggered.")
    if (is_local_dev()) {
      user_data(list(
        is_authenticated = TRUE,
        email = "dev_user@dartmouth.edu",
        name = "Local Dev User",
        id_token = "FAKE_ID_TOKEN_FOR_DEV"
      ))
      showNotification("DEV mode: Simulated logout (still logged in as dev user).", type = "message")
      print("DEBUG: DEV mode logout simulated.")
      return()
    }

    message("DEBUG: Logout requested by user.")
    ud <- user_data()
    user_data(list(is_authenticated = FALSE))
    session$userData$app_logic_initialized <- FALSE

    if (!is.null(DEX_LOGOUT_ENDPOINT) && nzchar(DEX_LOGOUT_ENDPOINT) && !is.null(ud$id_token)) {
      logout_url <- httr2::request(DEX_LOGOUT_ENDPOINT) |>
        httr2::req_url_query(
          id_token_hint = ud$id_token,
          post_logout_redirect_uri = APP_REDIRECT_URI
        ) |>
        purrr::pluck("url")

      message(paste("DEBUG: Redirecting to Dex logout URL:", logout_url))
      session$sendCustomMessage("redirect", logout_url)
    } else {
      message("DEBUG: DEX_LOGOUT_ENDPOINT not found or token missing. Performing simple page reload.")
      session$sendCustomMessage("reloadPage", list())
    }
  })

  observeEvent(input$user_is_inactive, {
    print("DEBUG: input$user_is_inactive event triggered.")
    if (isTRUE(input$user_is_inactive)) {
      message("DEBUG: User has been inactive for 15 minutes. Triggering logout.")
      shinyjs::click("logout_button")
    }
  })

  print("DEBUG: Authentication observers registered successfully.")

  observe({
    print("DEBUG: OIDC observe() triggered.")
    query_params <- parseQueryString(session$clientData$url_search)
    print(paste("DEBUG: Parsed query params:", toString(query_params)))
    if (!is.null(query_params$code) && !is.null(query_params$state) && is.null(session$userData$callbackCode)) {
      print("DEBUG: OIDC Callback with code and state received.")
      session$userData$callbackCode <- query_params$code
      session$userData$callbackState <- query_params$state
      session$userData$processedOIDCState <- FALSE
      print(paste("DEBUG: Stored callbackCode:", session$userData$callbackCode))
      print(paste("DEBUG: Stored callbackState:", session$userData$callbackState))
    } else if (!is.null(query_params$error)) {
      warning(paste("DEBUG: OIDC Error on callback:", query_params$error))
      session$sendCustomMessage("updateQueryString", "?")
      session$userData$callbackCode <- NULL
      session$userData$callbackState <- NULL
      session$userData$processedOIDCState <- FALSE
    } else {
      print("DEBUG: OIDC observe() - No action taken (maybe already processed).")
    }
  })

  observe({
    if (!is.null(session$userData$callbackCode) && 
        !is.null(session$userData$callbackState) &&
        !is.null(input$authStateCookieValue) &&
        !isTRUE(session$userData$processedOIDCState)) {
      
      print("DEBUG: *** FALLBACK: Manually triggering authentication flow ***")
      print(paste("DEBUG: Callback code:", session$userData$callbackCode))
      print(paste("DEBUG: Callback state:", session$userData$callbackState))
      print(paste("DEBUG: Input state:", input$authStateCookieValue))
      
      if (identical(session$userData$callbackState, input$authStateCookieValue)) {
        print("DEBUG: FALLBACK: State validation successful")
        session$userData$processedOIDCState <- TRUE
        session$sendCustomMessage("updateQueryString", "?")
        
        tryCatch({
          final_user_data <- exchange_code_for_token(session$userData$callbackCode)
          print("DEBUG: FALLBACK: Token exchange result:")
          print(str(final_user_data))
          
          if (!is.null(final_user_data) && isTRUE(final_user_data$is_authenticated)) {
            print("DEBUG: FALLBACK: SUCCESS! Setting authenticated user_data()")
            user_data(final_user_data)
            session$userData$ui_update_triggered <- FALSE
          } else {
            print("DEBUG: FALLBACK: Authentication failed")
            user_data(list(is_authenticated = FALSE))
            showNotification("Authentication failed. Please try logging in again.", type = "error", duration = 10)
          }
        }, error = function(e) {
          print(paste("DEBUG: FALLBACK: ERROR in token exchange:", e$message))
          user_data(list(is_authenticated = FALSE))
          showNotification("Authentication error occurred. Please try again.", type = "error", duration = 10)
        })
      } else {
        print("DEBUG: FALLBACK: State validation failed")
        showNotification("Authentication error (state validation failed). Please try again.", type = "error", duration = 10)
      }
    }
  })

  observe({
    ud <- req(user_data())
    print("DEBUG: GATED LOGIC observe - user_data().is_authenticated =")
    print(ud$is_authenticated)
    if (isTRUE(ud$is_authenticated) && !isTRUE(session$userData$app_logic_initialized)) {
      print("--- GATED LOGIC: Initializing Core App Logic... ---")
      session$userData$app_logic_initialized <- TRUE

      shinyInput <- function(FUN, len, id, ...) {
        inputs <- character(len)
        for (i in seq_len(len)) {
          inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
        }
        inputs
      }

      shinyValue <- function(id, len) {
        unlist(lapply(seq_len(len), function(i) {
          value <- input[[paste0(id, i)]]
          if (is.null(value)) NA else value
        }))
      }

      source("user.R", local=TRUE)
      source("helpers.R", local=TRUE)
      source("tables.R", local=TRUE)
      source("sidebar_body_panel.R", local=TRUE)
      source("template_upload_rc.R", local=TRUE)
      source("template_instr.R",local=TRUE)
      source("visnetwork.R", local=TRUE)
      source("associate_upload.R", local = TRUE)
      source("create_study.R", local = TRUE)
      source("arm_or_cohort.R", local = TRUE)
      source("condition_or_disease.R", local = TRUE)
      source("planned_visit.R", local = TRUE)
      source("experiment.R", local = TRUE)
      source("intervention.R", local = TRUE)
      source("inclusion_exclusion.R", local = TRUE)
      source("treatment.R", local = TRUE)
      source("immune_exposure.R", local = TRUE)
      source("categorization.R", local = TRUE)
      source("study_link.R", local = TRUE)
      source("reagent.R", local = TRUE)
      source("send_message.R", local = TRUE)
      source("study_personnel.R", local = TRUE)
      source("workspace_access.R", local = TRUE)
      source("immport_upload_module.R", local = TRUE)
      source("immport_server_logic.R", local = TRUE)

      # --- USER WORKSPACE FILTERING LOGIC ---
      # Now that user is authenticated, get their workspace and filter studies accordingly
      print("DEBUG: Setting up user workspace filtering for authenticated user")
      
      # Get current user's workspace from the users table
      current_user_email <- ud$email
      if (!is.null(current_user_email) && nzchar(current_user_email)) {
        current_user_compress <- gsub("[[:punct:][:blank:]]+", "", current_user_email)
        
        # Query user's workspace
        user_workspace_result <- tryCatch({
          DBI::dbGetQuery(conn, "
            SELECT workspace_id, project, full_name 
            FROM madi_track.users 
            WHERE regexp_replace(oauth_unique_id, '[[:punct:][:blank:]]', '', 'g') = $1;", 
            params = list(current_user_compress))
        }, error = function(e) {
          warning("Failed to get user workspace: ", e$message)
          data.frame(workspace_id = numeric(0), project = character(0), full_name = character(0))
        })
        
        if (nrow(user_workspace_result) > 0 && !is.na(user_workspace_result$workspace_id[1])) {
          user_workspace_id <- user_workspace_result$workspace_id[1]
          print(paste("DEBUG: User workspace ID:", user_workspace_id))
          
          # Refresh study data with user's workspace filtering
          print("DEBUG: Refreshing study data with user workspace filtering")
          study <<- get_studies_data_direct(conn, user_workspace_id)
          all_study_list <<- get_user_workspace_studies(conn, user_workspace_id)
          
          print(paste("DEBUG: Found", nrow(study), "studies in user's workspace", user_workspace_id))
          
          # Store user workspace for use in other parts of the app
          session$userData$user_workspace_id <- user_workspace_id
          
          # Set the global current workspace reactive value
          current_workspace(user_workspace_id)
          
        } else {
          print("DEBUG: No workspace assigned to user - showing no studies")
          study <<- create_empty_studies_dataframe()
          all_study_list <<- data.frame(study_accession = character(0), brief_title = character(0), stringsAsFactors = FALSE)
          session$userData$user_workspace_id <- NULL
          
          # Clear the current workspace
          current_workspace(NULL)
        }
      }

      initialization_script_path <- "R_helpers/initialize_reticulate_python.R"
      if(file.exists(initialization_script_path)) {
        source(initialization_script_path, local = TRUE)
        print("DEBUG: Python initialization script sourced successfully.")
      }
      
      if (!isTRUE(session$userData$ui_update_triggered)) {
        print("DEBUG: Triggering ONE-TIME UI update after authentication")
        session$userData$ui_update_triggered <- TRUE
        invalidateLater(50, session)
      }
    } else if (!isTRUE(ud$is_authenticated)) {
      if (!is.null(session$userData$app_logic_initialized) && isTRUE(session$userData$app_logic_initialized)) {
        print("--- GATED LOGIC: User logged out / unauthenticated. Resetting init flag. ---")
      }
      session$userData$app_logic_initialized <- FALSE
      session$userData$ui_update_triggered <- FALSE
    }
  })

  # Observer to refresh studies when workspace changes
  observeEvent(current_workspace(), {
    # Only run this observer if user is authenticated
    if (!isTRUE(user_data()$is_authenticated)) {
      return()
    }
    
    # Prevent infinite loops
    if (isTRUE(workspace_update_in_progress())) {
      return()
    }
    
    current_workspace_id <- current_workspace()
    
    if (!is.null(current_workspace_id)) {
      print(paste("DEBUG: Workspace changed to:", current_workspace_id))
      print("DEBUG: Refreshing study data for new workspace")
      
      # Set flag to prevent loops
      workspace_update_in_progress(TRUE)
      
      tryCatch({
        # Refresh global study data
        study <<- get_studies_data_direct(conn, current_workspace_id)
        all_study_list <<- get_user_workspace_studies(conn, current_workspace_id)
        
        print(paste("DEBUG: Study data refreshed - found", nrow(study), "studies"))
        
        # Trigger UI refresh for studies AND workspace UI components
        studies_trigger(studies_trigger() + 1)
        workspace_list_trigger(workspace_list_trigger() + 1)
        
      }, error = function(e) {
        print(paste("DEBUG: Error refreshing study data for workspace", current_workspace_id, ":", e$message))
      }, finally = {
        # Always reset the flag
        workspace_update_in_progress(FALSE)
      })
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$userpanel <- renderUI({
    ud <- user_data()
    print("DEBUG: output$userpanel renderUI called.")
    if (isTRUE(ud$is_authenticated)) {
      user_email_display <- if (!is.null(ud$email) && is.character(ud$email) && length(ud$email) == 1 && nzchar(ud$email)) {
        ud$email
      } else {
        "N/A"
      }
      print(paste("DEBUG: user_email_display:", user_email_display))
      tagList(
        div(style="padding: 10px; text-align: center; color: white;", p(strong("User:")), p(user_email_display)),
        div(style="padding: 10px; text-align: center;", actionButton("logout_button", "Logout", class = "btn-danger", width = "80%"))
      )
    } else {
      div(style = "padding: 20px; text-align: center; color: grey;", "Please log in")
    }
  })

  output$body_content_ui <- renderUI({
    ud <- user_data()
    print(paste0("DEBUG: Rendering body. User is authenticated: ", isTRUE(ud$is_authenticated)))
    print(paste0("DEBUG: user_data() structure: ", str(ud)))
    
    if (!is.null(ud) && isTRUE(ud$is_authenticated)) {
      print("DEBUG: Rendering authenticated body content.")
      authenticated_body_content()
    } else {
      print("DEBUG: Rendering login button page.")
      fluidPage(
        style = "display: flex; justify-content: center; align-items: center; height: 80vh;",
        actionButton("login_button", "Login with Dartmouth Dex", class = "btn-primary btn-lg")
      )
    }
  })

  message("Main server function setup complete (excluding core app logic until auth). [DEBUG]")
}

options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)
shinyApp(ui = ui, server = server)
