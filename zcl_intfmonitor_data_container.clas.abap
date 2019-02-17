"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Data container</p>
CLASS zcl_intfmonitor_data_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Data</p>
      BEGIN OF mtyp_s_data,
        id       TYPE string,
        data_ref TYPE REF TO data,
        obj_ref  TYPE REF TO object,
        type     TYPE typename,
      END   OF mtyp_s_data .
    TYPES:
      "! <p class="shorttext synchronized" lang="en">DataContainer</p>
      mtyp_t_data TYPE STANDARD TABLE OF mtyp_s_data WITH KEY id .

    "! <p class="shorttext synchronized" lang="en">DataContainer</p>
    DATA mt_data TYPE mtyp_t_data READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Adds data to DataContainer</p>
    "!
    "! @exception error | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS add
      IMPORTING
        id_id   TYPE string
        id_data TYPE any
      EXCEPTIONS
        error .
    "! <p class="shorttext synchronized" lang="en">Gets data from DataContainer</p>
    "!
    "! @parameter id_id     | <p class="shorttext synchronized" lang="en">Parameter Id</p>
    "! @parameter ed_data   | <p class="shorttext synchronized" lang="en">Parameter valu</p>
    "! @parameter ed_type   | <p class="shorttext synchronized" lang="en">Name of Dictionary Type</p>
    "! @parameter er_data   | <p class="shorttext synchronized" lang="en">Reference to parameter value</p>
    "! @exception not_found | <p class="shorttext synchronized" lang="en">Not Found</p>
    "! @exception error     | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS get
      IMPORTING
        id_id   TYPE string
      EXPORTING
        ed_data TYPE any
        ed_type TYPE typename
        er_data TYPE REF TO data
      EXCEPTIONS
        not_found
        error .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_INTFMONITOR_DATA_CONTAINER IMPLEMENTATION.
ENDCLASS.
