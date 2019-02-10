INTERFACE zif_intfmonitor
  PUBLIC .

  DATA:
    md_guid     TYPE zzeintfprocguid READ-ONLY,
    ms_detail   TYPE zeintfmonitor_detail READ-ONLY,
    ms_detail_x TYPE zeintfmonitor_detail_x READ-ONLY.

  "! Initializes the interface monitor
  "! @parameter id_guid   | Communication unique indentifier
  "! @parameter is_detail | Information regarding the communication process
  METHODS initialize IMPORTING id_guid   TYPE zzeintfprocguid
                               is_detail TYPE zeintfmonitor_detail.

  "! Adds a new parameter into the interface monitor
  "! @parameter id_param | Parameter name
  "! @parameter ix_val   | Parameter value
  METHODS add_parameter IMPORTING id_param TYPE any
                                  ix_val   TYPE any.
  "! Retrieves an existing parameter value
  "! @parameter id_param | Parameter name
  "! @parameter ex_val   | Parameter value
  "! @parameter ed_type  | Name of Dictionary Type
  "! @parameter er_data  | Reference to parameter value
  METHODS get_parameter IMPORTING id_param TYPE any
                        EXPORTING ex_val   TYPE any
                                  ed_type  TYPE typename
                                  er_data  TYPE REF TO data
                        RAISING   zcx_intfmonitor.

  "! Returns detail information regarding the type of communication
  "! @parameter rs_detail | Detail information
  METHODS get_detail RETURNING VALUE(rs_detail) TYPE zeintfmonitor_detail.

  "! Returns detail information regarding the type of communication with texts depending on logon language
  "! @parameter rs_detail_x | Detail information
  METHODS get_detail_x RETURNING VALUE(rs_detail_x) TYPE zeintfmonitor_detail_x.

  "! Reads a comunication execution
  METHODS read
    RAISING
      zcx_intfmonitor.
  "! Stores a communication execution
  METHODS store
    RAISING
      zcx_intfmonitor.

ENDINTERFACE.
