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
  - Enter program name (must start with Z or Y for custom programs)
  - Click "Create" for new program
  - Select program type (Executable)
  - Enter title
  - Choose package (ZKAR)
  - Choose request (Kerem Ar Abap Request)
  - Use "Direct Processing" for immediate activatio

### SM12 - Lock Management
- Purpose: Manage system locks
- Steps:
  - View List (f8)
  - Select entries to delete
  - Use Delete button(trash icon) to remove locks

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
     - Select "Search Help"
     - Enter name
     - Select elementary search help
     - Add description
     - Add the table where you will get help in "Selection method"
     - Add data elements from that table and check both checkboxes(in most cases)
     
     

### SE16/SE16N - General Table Display
- Purpose: View database table contents and structure
- Features:
  - Set filters using "Selection Criteria"
  - Export data to local file
  - View table structure
- Steps:
  - Enter table name
- Click "Execute" or press F8
 
### SE16 - Data Browser
- Purpose: View database table contents and add new entries
- Features:
  - Create Entries
  - View table contents
- Steps:
  - Enter table name
  - Click "Create Entries" button on top left
  - Click "New Entries" button on top left

### SE24 - Class Builder
- Purpose: Create and modify ABAP Objects
- Steps:
  - Enter class name (ZCL_* or YCL_*)
  - Add description
  - Choose package (ZKAR)
  - Choose request (Kerem Ar Abap Request)
 
- Will be continued...

### SOST - SAPconnect Send Orders
- Purpose: Monitor email sending status
- Features:
  - View sent emails
  - Check failed deliveries
  - Resend failed messages
- Steps:
  - Enter "0" on Maximum No. of Hits
  - Select row and click display document(glasses icon)

### SmartForms
- Purpose: Create print forms
- Steps:
  1. Create form:
     - Enter name (Z* or Y*)
     - Form interface and global definitions will be continued...
  2. Form Painter:(add video link)
     - Add windows
     - Insert text elements
     - Define tables 

### SMW0 - SAP Web Repository
- Purpose: Manage and store web objects like images, stylesheets, and HTML files within the SAP system.
- Steps:
  - Select binary data for excel template
  - Enter your package(ZKAR)
  - Click create(f5) on top left

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

### SM30 - Table Maintenance Generator(video link)
- Purpose: Create maintenance views
- Steps:
  1. Enter table name
  2. Generate maintenance view
  3. Define:
     - Fields to display
     - Authorization groups
     - Screen layout
  4. Activate view

### SNRO - Number Range Objects(video link)
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

### SE93 - Transaction Code Maintenance(video)
- Purpose: Create/modify transaction codes
- Steps:
  1. Enter transaction code (Z* or Y*)
  2. Define:
     - Program name
     - Screen number
     - Transaction type
  3. Set authorization group
  4. Save and generate

### ZABAPGIT - Git Integration(video)
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

## Database Operations
1. Data Declarations
```
DATA:
  gt_struc TYPE TABLE OF zkar_odev5_s01,  " Internal table to store multiple records
  gs_struc TYPE zkar_odev5_s01.           " Work area to store a single record
```
gt_struc is declared as an internal table that will hold multiple entries of type zkar_odev5_s01.
gs_struc is declared as a work area to hold a single entry of type zkar_odev5_s01.

2. SELECT Statement
The SELECT statement retrieves data from multiple tables (T001, T012, BNKA, T012K, SKAT, T012T) with INNER and LEFT JOINs.

```
SELECT
    t001~bukrs,    " Company Code from table T001
    t001~butxt,    " Company Name from table T001
    bnka~banka,    " Bank Name from table BNKA
    t012~hbkid,    " Bank ID from table T012
    t012k~hktid,   " Payment method ID from table T012K
    t012t~text1,   " Text for the payment method from table T012T
    t012k~waers,   " Currency from table T012K
    t012k~hkont,   " Account from table T012K
    skat~txt50     " Description from table SKAT
```
This part selects specific fields from each table (e.g., bukrs, butxt, banka, etc.).
The \~ symbol is used to reference fields in specific tables.
3. INNER JOINs
```
INNER JOIN t012 ON t012~bukrs = t001~bukrs
INNER JOIN bnka ON bnka~banks = t012~banks AND
                   bnka~bankl = t012~bankl
INNER JOIN t012k ON t012k~bukrs = t012~bukrs AND
                     t012k~hbkid = t012~hbkid
```
These INNER JOINs combine records from different tables based on matching fields:
T001 and T012 are joined on bukrs.
T012 and BNKA are joined on banks and bankl.
T012 and T012K are joined on bukrs and hbkid.
4. LEFT JOINs
```
LEFT JOIN skat ON skat~spras = 'TR' AND
                   skat~ktopl = t001~ktopl AND
                   skat~saknr = t012k~hkont
LEFT JOIN t012t ON t012t~bukrs = t012k~bukrs AND
                   t012t~hbkid = t012k~hbkid AND
                   t012t~hktid = t012k~hktid AND
                   t012t~spras = 'TR'
```
LEFT JOIN ensures that even if there's no match, the result will still include the row from the left table:
SKAT is joined with T001 and T012K based on spras, ktopl, and saknr.
T012T is joined on bukrs, hbkid, hktid, and spras.
5. INTO CORRESPONDING FIELDS OF TABLE
```
INTO CORRESPONDING FIELDS OF TABLE @gt_struc
```
This part inserts the data retrieved from the SELECT statement into the internal table gt_struc. The CORRESPONDING FIELDS ensures that the selected fields are mapped to the corresponding fields of the internal table gt_struc.
6. WHERE Clause with Filter Conditions
```
WHERE t001~bukrs  EQ @p_bukrs    " Filter by Company Code
  AND t012~hbkid  IN @s_hbkid    " Filter by Bank ID (Selection option)
  AND t012k~waers IN @s_waers.   " Filter by Currency (Selection option)
```
This WHERE clause applies several filters:
t001\~bukrs must match the value in p_bukrs (a parameter).
t012\~hbkid must be one of the values in s_hbkid (a selection option).
t012k\~waers must be one of the values in s_waers (another selection option).

## Email Integration

### Basic Email Setup and Sending

1. **Required Data Declarations**
```
" Email related data structures
TYPES: BEGIN OF zemail_structure,
name_text TYPE adrp-name_text,
smtp_addr TYPE adr6-smtp_addr,
END OF zemail_structure.
DATA: it_email TYPE TABLE OF zemail_structure,
rv_email TYPE zemail_structure,
lv_recipient TYPE string.
```
2. **Getting Email Recipients from Database**
```
" Select users who are marked for email receipt
SELECT zusername
FROM zkar_mail_odev06
INTO TABLE @DATA(selected_users)
WHERE checkbox = 'X'.
" Get email addresses for selected users
SELECT SINGLE adrp~name_text, adr6~smtp_addr
FROM usr21
LEFT JOIN adr6
ON usr21~addrnumber = adr6~addrnumber
AND usr21~persnumber = adr6~persnumber
LEFT JOIN adrp
ON usr21~persnumber = adrp~persnumber
AND adrp~date_from = @mc_date_from
AND adrp~nation = ''
WHERE usr21~bname = @selected_user-zusername
AND adr6~smtp_addr IS NOT INITIAL
INTO @rv_email.
```
3. **Sending Email with Attachment**
```
" Create mail object
DATA: lo_mail TYPE REF TO cl_mail.
CREATE OBJECT lo_mail.
" Send email with attachment
lo_mail->send_email(
EXPORTING
iv_subject = 'TCMB Kur Bilgileri' " Email subject
iv_body = 'TCMB günlük kur bilgileri ektedir.' " Email body
iv_recipient = lv_recipient " Recipient email address
).
" Check if email was sent successfully
IF sy-subrc IS INITIAL.
MESSAGE 'Message sent' TYPE 'S'.
ELSE.
MESSAGE 'Mesaj gönderilemedi.' TYPE 'E'.
ENDIF.
```
4. **Adding Excel Attachment to Email**
```
" Create ALV object for Excel
DATA: salv_table TYPE REF TO cl_salv_table.
TRY.
" Create ALV instance
cl_salv_table=>factory(
IMPORTING
r_salv_table = table
CHANGING
t_table = gt_list ).
" Prepare email subject with date
DATA: lv_subject TYPE bcs_filename.
lv_subject = |{ p_tarih } Günü Kur Bilgisi.xlsx|.
" Convert table to XML (Excel format)
DATA: v_xstring TYPE xstring.
v_xstring = table->to_xml( if_salv_bs_xml=>c_type_xlsx ).
" Add attachment to email
msg->add_attachment(
EXPORTING
iv_doctype = 'EXT'
iv_filename = lv_subject
iv_contents_bin = v_xstring ).
CATCH cx_salv_msg.
" Handle exceptions
ENDTRY.
```
      
