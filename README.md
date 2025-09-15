# Data Portal

The portal provides an interactive interface for data visualization, study management, and file uploads with secure authentication.

## Features

- **Interactive Data Visualization**: Network-based exploration of studies, experiments, and related data
- **Study Management**: Create, edit, and manage study information, arms/cohorts, and protocols
- **File Upload System**: Secure file upload to AWS S3 with metadata tracking
- **Authentication**: OAuth-based authentication via Dartmouth Dex
- **Template System**: Support for data template uploads and validation
- **Multi-user Support**: Workspace-based access control
- **Database Integration**: PostgreSQL backend with comprehensive data model
- **Python Integration**: ImmPort data processing and template validation

## Prerequisites

- Docker and Docker Compose
- R 4.0+ (if running locally)
- Python 3.8+ with reticulate support
- PostgreSQL database access
- AWS S3 bucket access
- Dartmouth Dex OAuth credentials (OIDC)

## Quick Start with Docker

1. **Clone the repository**

2. **Set up environment variables**
   Create a `.env` file in the project root with the following variables:

   ```bash
   # Database Configuration (R Components - lowercase)
   db=postgres
   db_host=
   db_port=
   db_userid_x=
   db_pwd_x=

   # Database Configuration (Python Components - UPPERCASE)
   DB_USER=
   DB_PASSWORD=
   DB_HOST=
   DB_PORT=
   DB_NAME=

   # AWS S3 Configuration
   AWS_ACCESS_KEY_ID=your-access-key
   AWS_SECRET_ACCESS_KEY=your-secret-key
   AWS_DEFAULT_REGION=eu-west-3
   S3_BUCKET_NAME=madi-data-portal-s3-paris

   # Application Configuration
   HOSTNAME=your-app-hostname
   LOCAL_DEV=0

   # OIDC/Dex Configuration
   DEX_ISSUER=https://your-oidc-issuer.com
   DEX_CLIENT_ID=your-client-id
   DEX_CLIENT_SECRET=your-client-secret
   APP_REDIRECT_URI=https://your-app-domain.com/callback

   # Optional: File Upload Path
   upload_file_path=/data/uploaded_templates

   # Optional: ImmPort Credentials (if using ImmPort integration)
   ImmPortUsername=your-immport-username
   ImmPortPassword=your-immport-password
   ```

3. **Run with Docker**
   ```bash
   docker-compose up -d
   ```

4. **Access the application**
   Open your browser and navigate to `http://localhost:3838`

## Local Development Setup

### R Dependencies

The application requires several R packages. Install them using:

```r
install.packages(c(
  "shiny", "shinyjs", "shinyalert", "shinydashboard", "shinyWidgets",
  "visNetwork", "RPostgres", "DBI", "DT", "sqldf", "datamods",
  "stringi", "tidyverse", "shinyFiles", "readxl", "openxlsx",
  "rhandsontable", "sendmailR", "reactable", "glue", "data.table",
  "httr2", "jose", "openssl", "jsonlite", "urltools", "aws.s3",
  "reticulate"
))
```

### Database Setup

The application expects a PostgreSQL database with the following schemas:
- `madi_dat`: Main data tables
- `madi_track`: User tracking and authentication
- `madi_meta`: Metadata and configuration

### Running Locally

1. **Set environment variables**
   Create a `.Renviron` file in the project root with the required variables.

2. **Install dependencies**
   ```r
   source("install_packages.R")  # if available
   ```

3. **Run the application**
   ```r
   shiny::runApp("madi-shiny-front/app.R", host = "0.0.0.0", port = 3838)
   ```

## Environment Variables Reference

### Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `db` (R) / `DB_NAME` (Python) | Database name | `postgres` |
| `db_host` (R) / `DB_HOST` (Python) | Database host | `db_link` |
| `db_port` (R) / `DB_PORT` (Python) | Database port | `port` |
| `db_userid_x` (R) / `DB_USER` (Python) | Database username | `user` |
| `db_pwd_x` (R) / `DB_PASSWORD` (Python) | Database password | `xyz` |
| `AWS_ACCESS_KEY_ID` | AWS access key | `AKIAIOSFODNN7EXAMPLE` |
| `AWS_SECRET_ACCESS_KEY` | AWS secret key | `wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY` |
| `AWS_DEFAULT_REGION` | AWS region | `eu-west-3` |
| `S3_BUCKET_NAME` | S3 bucket for file uploads | `madi-data-portal-s3-paris` |
| `DEX_ISSUER` | Dex OAuth issuer URL | `https://dex.example.com` |
| `DEX_CLIENT_ID` | OAuth client ID | `madi-portal` |
| `DEX_CLIENT_SECRET` | OAuth client secret | `your-secret` |
| `APP_REDIRECT_URI` | OAuth redirect URI | `https://portal.example.com/callback` |

### Optional Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `LOCAL_DEV` | Enable local development mode | `0` |
| `upload_file_path` | Local file upload path | `/data/uploaded_templates` |
| `ImmPortUsername` | ImmPort integration username | - |
| `ImmPortPassword` | ImmPort integration password | - |
| `HOSTNAME` | Application hostname | - |

### Important Note on Environment Variables

The application uses both R and Python components that require different naming conventions:

- **R Components**: Use lowercase with underscores (e.g., `db_host`, `db_userid_x`)
- **Python Components**: Use UPPERCASE with underscores (e.g., `DB_HOST`, `DB_USER`)

Both sets of variables are required for the application to function properly:
- R variables are used by the Shiny application and database connections
- Python variables are used by the database engine for ImmPort integration and template processing

## Application Structure

```
madi-shiny-group-madi-data-portal/
├── madi-shiny-front/           # Main Shiny application
│   ├── app.R                   # Main application file
│   ├── auth_config.R           # Authentication configuration
│   ├── helpers.R               # Helper functions
│   ├── user.R                  # User management
│   ├── *.R                     # Various module files
│   ├── R_helpers/              # R helper scripts
│   │   └── initialize_reticulate_python.R
│   └── www/                    # Static assets
├── Dockerfile                  # Docker configuration
├── docker-compose.yml          # Docker Compose setup
├── .env.example               # Environment variables template
└── README.md                  # This file
```

## Key Features

### Authentication
- OAuth 2.0 integration with Dartmouth Dex
- Session management with inactivity timeout
- User workspace isolation

### Data Management
- Study creation and management
- Protocol and file uploads
- Template-based data entry
- S3 integration for file storage

### Visualization
- Interactive network graphs using visNetwork
- Tabular data display with DT/reactable
- Dynamic UI generation

### Python Integration
- ImmPort data processing
- Template validation and transformation
- Database engine connectivity

### Security
- Environment-based configuration
- Secure file upload handling
- Database connection pooling
- CSRF protection

## Development

### Adding New Modules

1. Create a new R file in `madi-shiny-front/`
2. Follow the existing pattern for UI/server separation
3. Source the file in `app.R` within the authenticated logic block

### Database Schema

The application uses a multi-schema PostgreSQL database:
- Tables are accessed through the `update_db()` helper function
- Schema is automatically handled based on table names
- Connection pooling is managed centrally

### Python Environment

The application initializes a Python environment using reticulate:
- Python initialization occurs after user authentication
- Database connections use the UPPERCASE environment variables
- Progress modal shows during Python environment setup

### Testing

Set `LOCAL_DEV=1` to enable local development mode with:
- Bypassed authentication
- Mock user data
- Simplified file handling

## Deployment

### Docker Production Deployment

1. **Build the image**
   ```bash
   docker build -t madi-data-portal .
   ```

2. **Run with environment file**
   ```bash
   docker run -d --env-file .env -p 3838:3838 madi-data-portal
   ```

### Environment-specific Configuration

- **Development**: Use `LOCAL_DEV=1` for testing
- **Staging**: Configure with staging database and S3 bucket
- **Production**: Ensure all security variables are properly set

## Troubleshooting

### Common Issues

1. **Database Connection Errors**
   - Verify both R and Python database credentials
   - Check that schemas `madi_dat` and `madi_track` exist
   - Ensure network connectivity to database host

2. **Authentication Failures**
   - Verify Dex configuration and redirect URIs
   - Check that OAuth credentials are correct

3. **File Upload Issues**
   - Verify AWS credentials and S3 bucket permissions
   - Check file size limits (default: 100MB)

4. **Python Environment Issues**
   - Check that reticulate can initialize Python
   - Verify Python database engine can connect with UPPERCASE variables
   - Check Python dependencies are installed

5. **Permission Errors**
   - Ensure user has appropriate workspace access
   - Check database user permissions

### Logs

Application logs are available through:
- Docker: `docker logs container-name`
- R Console: Check for printed debug messages
- Browser: Console for JavaScript errors

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## License

[Add your license information here]

## Support

For support and questions, please contact the MADI development team or create an issue in the GitHub repository.

## Changelog

### Version 1.0.0
- Initial release
- OAuth authentication
- Study management
- File upload system
- Interactive visualization
- Python integration for ImmPort processing
