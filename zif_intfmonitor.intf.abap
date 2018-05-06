*"* components of interface ZIF_INTFMONITOR
"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor</p>
INTERFACE zif_intfmonitor
  PUBLIC .

  TYPE-POOLS abap .

  "! <p class="shorttext synchronized" lang="en">Process Guid</p>
  DATA md_guid TYPE zzeintfprocguid READ-ONLY .
  "! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Interface details</p>
  DATA ms_detail TYPE zeintfmonitor_detail READ-ONLY .

  "! <p class="shorttext synchronized" lang="en">Adds interface parameter to monitor application</p>
  "!
  "! @parameter id_param | <p class="shorttext synchronized" lang="en">Parameter</p>
  "! @parameter ix_val   | <p class="shorttext synchronized" lang="en">Parameter value</p>
  METHODS add_parameter
    IMPORTING
      !id_param TYPE zzeintfdatapar
      !ix_val   TYPE any .
  "! <p class="shorttext synchronized" lang="en">Get detail</p>
  "!
  "! @parameter rs_detail | <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Interface details</p>
  METHODS get_detail
    RETURNING
      VALUE(rs_detail) TYPE zeintfmonitor_detail .
  "! <p class="shorttext synchronized" lang="en">Get Detail with descriptions</p>
  "!
  "! @parameter rs_detail_x | <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Interface detail with description</p>
  METHODS get_detail_x
    RETURNING
      VALUE(rs_detail_x) TYPE zeintfmonitor_detail_x .
  "! <p class="shorttext synchronized" lang="en">Gets interface parameter from monitof application</p>
  "!
  "! @parameter id_param | <p class="shorttext synchronized" lang="en">Parameter</p>
  "! @parameter er_data  | <p class="shorttext synchronized" lang="en">Reference to parameter val</p>
  "! @parameter ex_val   | <p class="shorttext synchronized" lang="en">Parameter value</p>
  METHODS get_parameter
    IMPORTING
      !id_param TYPE zzeintfdatapar
    EXPORTING
      !er_data  TYPE REF TO data
      !ex_val   TYPE any .
  "! <p class="shorttext synchronized" lang="en">Reads interface from monitor application</p>
  METHODS read .
  "! <p class="shorttext synchronized" lang="en">Stores interface into monitor application</p>
  METHODS store .
  "! <p class="shorttext synchronized" lang="en">Initializes instance</p>
  "!
  "! @parameter id_guid   | <p class="shorttext synchronized" lang="en">Process Guid</p>
  "! @parameter is_detail | <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Interface details</p>
  METHODS initialize
    IMPORTING
      !id_guid   TYPE zzeintfprocguid
      !is_detail TYPE zeintfmonitor_detail .
ENDINTERFACE.
