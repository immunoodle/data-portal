# Subject Table Structure & Migration Requirements

**Date:** December 1, 2025  
**Analysis for:** I-SPI Migration from `madi_results.xmap_sample` to `madi_dat` schema

---

## madi_dat.subject Table Structure

Based on analysis of the Python extraction scripts (`immport_template_generator.py`), the `subject` table has the following columns:

### Core Columns:
```sql
madi_dat.subject (
    subject_accession VARCHAR PRIMARY KEY,    -- e.g., 'SUB_1040'
    gender VARCHAR NOT NULL,                  -- REQUIRED: Male/Female/Other
    species VARCHAR,                          -- e.g., 'Homo sapiens', 'Mus musculus'
    ethnicity VARCHAR,                        -- For human subjects
    race VARCHAR,                             -- For human subjects
    race_specify VARCHAR,                     -- Additional race details
    description TEXT,                         -- Subject description
    strain VARCHAR,                           -- For animal subjects
    strain_characteristics VARCHAR,           -- For animal subjects
    workspace_id INTEGER NOT NULL             -- FK to workspace
)
```

### Key Constraints:
1. **`gender` is NOT NULL** - This is why our migration is failing!
2. **`workspace_id` is NOT NULL** - Required for all subjects
3. **NO `study_accession` column** - This lives in `arm_2_subject` instead

---

## madi_dat.arm_2_subject Junction Table

This table links subjects to studies AND arms/cohorts:

```sql
madi_dat.arm_2_subject (
    arm_accession VARCHAR NOT NULL,           -- FK to arm_or_cohort
    subject_accession VARCHAR NOT NULL,       -- FK to subject
    study_accession VARCHAR NOT NULL,         -- FK to study (THIS IS WHERE STUDY LIVES!)
    workspace_id INTEGER NOT NULL,
    min_subject_age NUMERIC,                  -- Subject age metadata
    max_subject_age NUMERIC,
    age_unit VARCHAR,
    age_event VARCHAR,
    age_event_specify VARCHAR,
    subject_phenotype VARCHAR,
    subject_location VARCHAR,
    PRIMARY KEY (arm_accession, subject_accession)
)
```

### Important Notes:
- **`study_accession` is stored here**, not in the `subject` table
- Contains additional subject metadata (age, phenotype, location)
- One subject can be in multiple arms (but typically isn't)

---

## Source Data: madi_results.xmap_sample

The source table has these relevant columns:

```
xmap_sample:
    patientid         -- Maps to subject_accession (with 'SUB_' prefix)
    timeperiod        -- Maps to planned_visit
    agroup            -- Maps to arm/cohort
    (NO gender field!)
    (NO species field!)
```

### Critical Gap:
**The source `xmap_sample` table does NOT contain `gender` or `species` information!**

---

## Solution Options

### Option 1: Get Gender/Species from xmap_subjects
The `madi_results.xmap_subjects` table likely has patient metadata:

```sql
SELECT * FROM madi_results.xmap_subjects 
WHERE xmap_patientid IN (SELECT DISTINCT patientid FROM madi_results.xmap_sample);
```

Expected columns in `xmap_subjects`:
- `xmap_patientid`
- `subject_accession` (already generated?)
- `arm_accession` (already mapped?)
- Likely: `gender`, `species`, `strain`, etc.

### Option 2: Default Values for I-SPI Study
If gender/species is consistent across the study:
- Default `gender` to a specific value (e.g., 'Not Specified' or actual study design)
- Default `species` to 'Homo sapiens' (if it's a human study)

### Option 3: Check mbaa_samples Table
The `mbaa_for_database` procedure joins `xmap_subjects`:

```sql
FROM madi_results.xmap_sample xs
JOIN madi_results.xmap_subjects subj ON xs.patientid = subj.xmap_patientid
```

So `xmap_subjects` definitely has the subject metadata we need.

---

## Recommended Migration Approach

### Step 1: Query xmap_subjects for Complete Subject Data
```r
subject_data <- DBI::dbGetQuery(source_conn,
  "SELECT 
     xmap_patientid,
     subject_accession,
     arm_accession,
     gender,
     species,
     ethnicity,
     race,
     race_specify,
     strain,
     strain_characteristics,
     description
   FROM madi_results.xmap_subjects
   WHERE xmap_patientid IN (
     SELECT DISTINCT patientid 
     FROM madi_results.xmap_sample
     WHERE study_accession = $1
   )",
  params = list(study_accession)
)
```

### Step 2: Insert Subjects with All Required Fields
```r
DBI::dbExecute(conn,
  "INSERT INTO madi_dat.subject 
   (subject_accession, workspace_id, gender, species, ethnicity, race, 
    race_specify, strain, strain_characteristics, description) 
   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)",
  params = list(
    subject_acc, 
    workspace_id, 
    gender,           # From xmap_subjects
    species,          # From xmap_subjects
    ethnicity,        # From xmap_subjects (optional)
    race,             # From xmap_subjects (optional)
    race_specify,     # From xmap_subjects (optional)
    strain,           # From xmap_subjects (optional)
    strain_characteristics,  # From xmap_subjects (optional)
    description       # From xmap_subjects (optional)
  )
)
```

### Step 3: Insert arm_2_subject Associations
```r
DBI::dbExecute(conn,
  "INSERT INTO madi_dat.arm_2_subject 
   (arm_accession, subject_accession, study_accession, workspace_id,
    min_subject_age, max_subject_age, age_unit, age_event, 
    age_event_specify, subject_phenotype, subject_location) 
   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)",
  params = list(
    arm_acc,
    subject_acc,
    study_accession,  # THIS is where study_accession goes!
    workspace_id,
    min_age,          # From xmap_subjects if available
    max_age,
    age_unit,
    age_event,
    age_event_specify,
    subject_phenotype,
    subject_location
  )
)
```

---

## Migration Data Flow Comparison

### Current (Broken) Flow:
```
xmap_sample (patientid) 
  → SUB_{patientid}
  → INSERT subject (subject_accession, workspace_id, study_accession ❌)
```

### Correct Flow:
```
xmap_sample (patientid)
  ↓
xmap_subjects (xmap_patientid = patientid) → Get gender, species, etc.
  ↓
INSERT subject (subject_accession, workspace_id, gender ✅, species, ...)
  ↓
INSERT arm_2_subject (arm_accession, subject_accession, study_accession ✅, ...)
```

---

## Next Actions Required

1. **Verify xmap_subjects structure** in source database
2. **Modify `insert_subjects()` function** to:
   - Join with `xmap_subjects` table
   - Include `gender` (required)
   - Include `species` and other optional fields
3. **Modify `insert_arm_subject_associations()` function** to:
   - Include `study_accession` parameter
   - Include age/phenotype/location metadata if available
4. **Test with source database connection** to verify data availability
