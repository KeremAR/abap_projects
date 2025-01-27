# ABAP Learning Projects Documentation

This repository contains various ABAP projects demonstrating different concepts and functionalities. Below is a comprehensive guide to help understand the implementations.

## Table of Contents

1. [Common T-Codes](#common-t-codes)
2. [Database Operations](#database-operations)
3. [ALV Implementations](#alv-implementations)
4. [Screen Management](#screen-management)
5. [Email Integration](#email-integration)
6. [File Handling](#file-handling)
7. [Data Visualization](#data-visualization)

## Common T-Codes

### SE38 - ABAP Editor
- Purpose: Create and edit ABAP programs
- Steps:
  1. Enter program name (must start with Z or Y for custom programs)
  2. Click "Create" for new program
  3. Select program type (Executable)
  4. Enter title
  5. Choose package (ZKAR)
  6. Choose request (Kerem Ar Abap Request)
  7. Use "Direct Processing" for immediate activatio

### SM12 - Lock Management
- Purpose: Manage system locks
- Steps:
  1. View List (f8)
  2. Select entries to delete
  3. Use Delete button(trash icon) to remove locks

### SE11 - ABAP Dictionary
- Purpose: Create/modify database objects
- Common Tasks:
  1. **Create Domain:**
     - Select "Domain"
     - Enter name (starts with Z* or Y* ends with "do")
     - Add description
     - Define data type (char, int4, dats) and length
     - Add value range if needed.(Fixed column for values, and description column)
  
  2. **Create Data Element:**
     - Select "Data Element"
     - Enter name
     - Add description
     - Link to domain or define type
     - Fill field labels(description on different lengths)
  
  3. **Create Table:**
     - Select "Database Table"
     - Enter name
     - Add description
     - Delivery class : A (Application table)
     - Table View Editing : Allowed
     - Add data elements and their field names(should be short)
     - Adding MANDT with checked key and index checkboxes is mandatory
     - Both checkboxes of the table key must be checked
    (Currency error fix: T001-WAERS)
       
    
  4. **Create Search Help:**
     -Select "Search Help"
     -Enter name
     -Select elementary search help
     -Add description
     -Add the table where you will get help in "Selection method"
     -Add data elements from that table and check both checkboxes(in most cases)
     
     

### SE16/SE16N - General Table Display
- Purpose: View database table contents and structure
- Features:
  - Set filters using "Selection Criteria"
  - Export data to local file
  - View table structure
- Steps:
  1. Enter table name
  2. Click "Execute" or press F8
 
### SE16 - Data Browser
- Purpose: View database table contents and add new entries
- Features:
  - Create Entries
  - View table contents
- Steps:
  1. Enter table name
  2. Click "Create Entries" button on top left
  3. Click "New Entries" button on top left

### SE24 - Class Builder
- Purpose: Create and modify ABAP Objects
- Steps:
  1. Enter class name (ZCL_* or YCL_*)
  2. Add description
  3. Choose package (ZKAR)
  4. Choose request (Kerem Ar Abap Request)
 
- Will be continued...

### SOST - SAPconnect Send Orders
- Purpose: Monitor email sending status
- Features:
  - View sent emails
  - Check failed deliveries
  - Resend failed messages
- Steps:
  1. Enter "0" on Maximum No. of Hits
  2. Select row and click display document(glasses icon)

### SmartForms
- Purpose: Create print forms
- Steps:
  1. Create form:
     - Enter name (Z* or Y*)
     - Define form attributes
  2. Design layout:
     - Add windows
     - Insert text elements
     - Define tables
  3. Activate and test

### SMW0 - MIME Repository
- Purpose: Manage MIME objects
- Common Uses:
  - Store images
  - Upload files
  - Manage web content
- Steps:
  1. Create/select folder
  2. Upload objects
  3. Set permissions

### SE37 - Function Builder
- Purpose: Create/modify function modules
- Steps:
  1. Enter function name (Z* or Y*)
  2. Define:
     - Import parameters
     - Export parameters
     - Tables
     - Exceptions
  3. Implement logic
  4. Test function

### SM30 - Table Maintenance Generator
- Purpose: Create maintenance views
- Steps:
  1. Enter table name
  2. Generate maintenance view
  3. Define:
     - Fields to display
     - Authorization groups
     - Screen layout
  4. Activate view

### SNRO - Number Range Objects
- Purpose: Manage number ranges
- Steps:
  1. Create number range object:
     - Enter object name
     - Define intervals
     - Set number range
  2. Maintain intervals:
     - Start/end numbers
     - Current number
     - External/internal

### SE93 - Transaction Code Maintenance
- Purpose: Create/modify transaction codes
- Steps:
  1. Enter transaction code (Z* or Y*)
  2. Define:
     - Program name
     - Screen number
     - Transaction type
  3. Set authorization group
  4. Save and generate

### ZABAPGIT - Git Integration
- Purpose: Version control for ABAP objects
- Features:
  - Repository management
  - Object versioning
  - Code sharing
- Steps:
  1. Install ZABAPGIT
  2. Create/clone repository
  3. Stage changes
  4. Commit and push

### Additional Tips:
- Use F1 help for detailed documentation
- Check authorization before creating objects
- Follow naming conventions (Z* or Y* for custom objects)
- Always activate objects after modification
- Use transaction variants for frequently used settings
