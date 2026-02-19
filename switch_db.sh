#!/bin/bash

# Script to switch between local and production database configurations
# Usage: ./switch_db.sh [local|prod]

DOCKER_ENV_FILE="docker.env"
BACKUP_FILE="docker.env.backup"

if [ "$1" = "local" ]; then
    echo "Switching to LOCAL database configuration..."
    
    # Backup current file
    cp "$DOCKER_ENV_FILE" "$BACKUP_FILE"
    
    # Update database configuration for local
    sed -i '' 's/^db=.*/db=madi_production_test/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^db_host=.*/db_host=host.docker.internal/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^db_port=.*/db_port=5432/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^db_userid_x=.*/db_userid_x=hardik/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^db_pwd_x=.*/db_pwd_x=/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_NAME=.*/DB_NAME=madi_production_test/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_HOST=.*/DB_HOST=host.docker.internal/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_PORT=.*/DB_PORT=5432/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_USER=.*/DB_USER=hardik/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_PASSWORD=.*/DB_PASSWORD=/' "$DOCKER_ENV_FILE"
    
    echo "✅ Switched to LOCAL database"
    
elif [ "$1" = "prod" ]; then
    echo "Switching to PRODUCTION database configuration..."
    
    # Backup current file
    cp "$DOCKER_ENV_FILE" "$BACKUP_FILE"
    
    # Update database configuration for production
    sed -i '' 's/^db=.*/db=postgres/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^db_host=.*/db_host=madi-dev-7605-db.c.dartmouth.edu/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^db_port=.*/db_port=5432/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^db_userid_x=.*/db_userid_x=d78039e/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^db_pwd_x=.*/db_pwd_x=abc123/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_NAME=.*/DB_NAME=postgres/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_HOST=.*/DB_HOST=madi-dev-7605-db.c.dartmouth.edu/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_PORT=.*/DB_PORT=5432/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_USER=.*/DB_USER=d78039e/' "$DOCKER_ENV_FILE"
    sed -i '' 's/^DB_PASSWORD=.*/DB_PASSWORD=abc123/' "$DOCKER_ENV_FILE"
    
    echo "✅ Switched to PRODUCTION database"
    
else
    echo "Usage: $0 [local|prod]"
    echo "  local - Switch to local database (madi_production_test on host.docker.internal)"
    echo "  prod  - Switch to production database (postgres on madi-dev-7605-db.c.dartmouth.edu)"
    exit 1
fi

echo ""
echo "Current database configuration:"
grep -E "^(db|DB_NAME|DB_HOST)" "$DOCKER_ENV_FILE"
echo ""
echo "Backup saved to: $BACKUP_FILE"
