"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor</p>
CLASS zcf_intfmonitor DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
*"* public components of class ZCF_INTFMONITOR
*"* do not include other source files here!!!

    "! <p class="shorttext synchronized" lang="en">Creates new Interface Monitor to store data</p>
    "!
    "! @parameter id_intfid                 | <p class="shorttext synchronized" lang="en">Interface Id</p>
    "! @parameter ro_instance               | <p class="shorttext synchronized" lang="en">SAP Interface Monitor</p>
    "! @exception not_found                 | <p class="shorttext synchronized" lang="en">Interface not found</p>
    "! @exception unable_to_create_instance | <p class="shorttext synchronized" lang="en">Unable to create instance</p>
    CLASS-METHODS new
      IMPORTING
        !id_intfid         TYPE zzeintfid
      RETURNING
        VALUE(ro_instance) TYPE REF TO zif_intfmonitor
      EXCEPTIONS
        not_found
        unable_to_create_instance .
    "! <p class="shorttext synchronized" lang="en">Retrieves existing Interface Monitor for reading data</p>
    "!
    "! @parameter id_guid                   | <p class="shorttext synchronized" lang="en">Process Guid</p>
    "! @parameter ro_instance               | <p class="shorttext synchronized" lang="en">SAP Interface Monitor</p>
    "! @exception not_found                 | <p class="shorttext synchronized" lang="en">Not found</p>
    "! @exception unable_to_create_instance | <p class="shorttext synchronized" lang="en">Unable to create instance</p>
    CLASS-METHODS find_by_guid
      IMPORTING
        !id_guid           TYPE zzeintfprocguid
      RETURNING
        VALUE(ro_instance) TYPE REF TO zif_intfmonitor
      EXCEPTIONS
        not_found
        unable_to_create_instance .
  PROTECTED SECTION.
*"* protected components of class ZCF_INTFMONITOR
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCF_INTFMONITOR
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcf_intfmonitor IMPLEMENTATION.


  METHOD find_by_guid.
    DATA: ls_head  TYPE zintfmonitor020,
          ls_custo TYPE zintfmonitor010.

    TRY.
        ls_head  = zcl_zintfmonitor020_read=>get_detail( id_guid ).
        ls_custo = zcl_zintfmonitor010_read=>get_detail( ls_head-intfid ).

        CREATE OBJECT ro_instance TYPE (ls_custo-clsname).

        ro_instance->initialize( EXPORTING id_guid   = ls_head-guid
                                           is_detail = ls_head-detail ).
*     Retrieve saved data
        ro_instance->read( ).

      CATCH cx_db2_not_found .
        MESSAGE e000(zintfmonitor) WITH id_guid RAISING not_found.
*     There is no Interface execution with id '&1'.
      CATCH cx_sy_create_object_error.
        MESSAGE e002(zintfmonitor) WITH ls_custo-clsname ls_head-intfid RAISING unable_to_create_instance.
*     Unable to create instance from class '&1' of interface '&2'.

    ENDTRY.



  ENDMETHOD.


  METHOD new.
    DATA: ls_custo  TYPE zintfmonitor010,
          ld_guid   TYPE zzeintfprocguid,
          ls_detail TYPE zeintfmonitor_detail.

    TRY.
        ls_custo = zcl_zintfmonitor010_read=>get_detail( id_intfid ).

        CREATE OBJECT ro_instance TYPE (ls_custo-clsname).

        ld_guid                = cl_system_uuid=>create_uuid_c32_static( ).
        ls_detail-intfid       = id_intfid.
        ls_detail-procdate     = sy-datum.
        ls_detail-proctime     = sy-uzeit.
        ls_detail-procby       = sy-uname.

        ro_instance->initialize( EXPORTING id_guid   = ld_guid
                                           is_detail = ls_detail ).

      CATCH cx_db2_not_found .
        MESSAGE e001(zintfmonitor) WITH id_intfid RAISING not_found.
*     Interface id '&1' does not exist.

      CATCH cx_sy_create_object_error.
        MESSAGE e002(zintfmonitor) WITH ls_custo-clsname id_intfid RAISING unable_to_create_instance.
*     Unable to create instance from class '&1' of interface '&2'.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
