# I-SPI Migration Issue: Missing Subject Metadata

**Date:** December 1, 2025  
**Context:** I-SPI to MADI Data Portal Migration  
**Severity:** Blocking migration execution

---

## Issue Summary

While implementing direct database-to-database migration for I-SPI data (bypassing the old procedure-based workflow), we discovered that **subject records cannot be created** because the source I-SPI database lacks required subject metadata fields.

---

## Technical Details

### The Problem

The target `madi_dat.subject` table requires the following columns:
- `subject_accession` (PK)
- `workspace_id` (NOT NULL)
- **`gender` (NOT NULL)** ⚠️
- `species` (optional but important)
- Plus: `ethnicity`, `race`, `strain`, etc. (all optional)

**However**, the I-SPI source database's `xmap_subjects` table only contains:
```sql
CREATE TABLE madi_results.xmap_subjects (
    xmap_patientid VARCHAR(15) NOT NULL,
    study_accession VARCHAR(15) NOT NULL,
    subject_accession VARCHAR(15),       -- Pre-generated
    arm_accession VARCHAR(15),            -- Pre-mapped
    agroup VARCHAR(15),
    patientid VARCHAR(15)
);
```

**No `gender`, `species`, `ethnicity`, `race`, or any other subject characteristics!**

### Why This Wasn't a Problem Before

The old workflow using stored procedures (`mbaa_for_database`, etc.) **never actually created subject records**. Those procedures only:
1. Created intermediate tables in `madi_results` schema
2. Generated `mbaa_samples` with pre-mapped `subject_accession` references
3. Expected subjects to already exist in `madi_dat.subject`

**Someone must have manually created the subject records separately**, or there was another process (Excel import? Manual SQL?) that we're not aware of.

### Why This IS a Problem Now

Our new direct-migration app attempts to:
1. Read patient IDs from I-SPI `xmap_sample` table
2. Create corresponding subject records in `madi_dat.subject`
3. **FAILS** because we can't provide the required `gender` field

---

## Questions for the Team

### 1. How Were Subjects Created Previously?
- Was there a manual step to create subjects before running the procedures?
- Is there a separate Excel template or SQL script for subject creation?
- Are subject records shared across studies, or created per-study?

### 2. What Are the Actual Subject Characteristics for I-SPI?
For the I-SPI study specifically:
- **Species:** Is this a human study? Animal study? Mixed?
- **Gender:** Is it single-gender? Mixed? Unknown?
- **Other metadata:** Do we have ethnicity, age ranges, or other demographic data somewhere else?

### 3. Where Does This Data Live?
If subject metadata exists somewhere:
- Is it in a different database/schema?
- Is it in documentation or study protocols?
- Is it in the original data submission from the lab?

---

## Proposed Solutions (Pending Team Input)

### Option A: Study-Level Defaults (Quick Fix)
Add configuration in the migration UI where users specify study-wide defaults:
```
Study Species: [Dropdown: Homo sapiens | Mus musculus | Other]
Default Gender: [Dropdown: Male | Female | Not Specified | Mixed]
```

**Pros:** Fast to implement, works for homogeneous studies  
**Cons:** May lose individual subject variation if it exists elsewhere

### Option B: External Metadata File
Create a subject metadata mapping file (CSV/Excel) that users upload:
```csv
patientid,gender,species,ethnicity,race,age_min,age_max
1001,Female,Homo sapiens,Not Hispanic,White,25,30
1002,Male,Homo sapiens,Not Hispanic,White,30,35
...
```

**Pros:** Preserves individual subject details, reusable for future migrations  
**Cons:** Requires manual data preparation

### Option C: Make Fields Optional (Temporary)
Request DBA to temporarily remove NOT NULL constraint on `gender`:
```sql
ALTER TABLE madi_dat.subject ALTER COLUMN gender DROP NOT NULL;
```

**Pros:** Unblocks migration immediately  
**Cons:** May violate ImmPort schema requirements, could cause issues downstream

### Option D: Pre-populate Subjects Separately
Continue the old pattern:
1. Manually create all subjects first (separate process)
2. Migration app skips subject creation, only creates arm associations and samples

**Pros:** Maintains existing workflow pattern  
**Cons:** Requires two-step process, not fully automated

---

## Immediate Action Items

1. **Confirm**: How were I-SPI subjects created in the original migration?
2. **Identify**: Where does subject demographic data exist (if anywhere)?
3. **Decide**: Which solution approach fits the team's workflow best?
4. **Document**: Whatever the answer is, so we don't lose this knowledge again!

---

## Impact Assessment

**Blocking:** Yes - cannot complete migration without resolution  
**Workaround Available:** Yes - multiple options depending on data availability  
**Timeline:** Can implement chosen solution within 1-2 hours once decision is made  
**Risk:** Low - this is a data availability issue, not a code bug

---

## Additional Context

### What We've Already Fixed
1. ✅ Removed `study_accession` from subject table (it belongs in `arm_2_subject`)
2. ✅ Implemented savepoint-based error handling for batch inserts
3. ✅ Documented complete subject table structure and relationships

### What's Still Working
- Connection to I-SPI database ✅
- Data preview and exploration ✅
- Timeperiod and Agroup mapping ✅
- All other table structures verified ✅

### What's Blocked
- Subject creation (and therefore full migration test)
- Cannot proceed to experiment sample creation until subjects exist

---

## Request

**Please advise on:**
1. Historical process for subject creation
2. Location of subject demographic data (if it exists)
3. Preferred solution approach (A, B, C, or D above)

Once we have clarity on the data source, we can implement the fix and complete testing within a few hours.

---

**Prepared by:** Harry & GitHub Copilot  
**Next Session:** Awaiting team input on data source and preferred approach
