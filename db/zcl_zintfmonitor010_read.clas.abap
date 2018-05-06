"! <p class="shorttext synchronized" lang="en">DB Framework for: ZINTFMONITOR010</p>
CLASS zcl_zintfmonitor010_read DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.
*"* public components of class ZCL_ZINTFMONITOR010_READ
*"* do not include other source files here!!!

    "! <p class="shorttext synchronized" lang="en">Delete Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_details
      IMPORTING
        !is_details TYPE zintfmonitor010
      RAISING
        cx_db2_not_found .
    "! <p class="shorttext synchronized" lang="en">Delete Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor010
      RAISING
        cx_db2_not_found .
    "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface Id</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    CLASS-METHODS get_detail
      IMPORTING
        !id_intfid       TYPE zintfmonitor010-intfid
          PREFERRED PARAMETER id_intfid
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor010
      RAISING
        cx_db2_not_found .
    "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter et_list   | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS get_list
      IMPORTING
        !id_intfid TYPE zintfmonitor010-intfid OPTIONAL
      EXPORTING
        !et_list   TYPE ztt_zintfmonitor010
      RAISING
        cx_db2_not_found .
    "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
    CLASS-METHODS init_buffer .
    "! <p class="shorttext synchronized" lang="en">Save Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_detail
      IMPORTING
        !is_details TYPE zintfmonitor010
      RAISING
        cx_db2_not_found .
    "! <p class="shorttext synchronized" lang="en">Save Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor010
      RAISING
        cx_db2_not_found .
    "! <p class="shorttext synchronized" lang="en">Find details with descriptions by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    CLASS-METHODS get_detail_x
      IMPORTING
        !id_intfid       TYPE zintfmonitor010-intfid
      RETURNING
        VALUE(rs_result) TYPE zeintfmonitor010_detail_x
      RAISING
        cx_db2_not_found .
  PROTECTED SECTION.
*"* protected components of class ZCL_ZINTFMONITOR010_READ
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_ZINTFMONITOR010_READ
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF mtyp_ranges,
        intfid TYPE RANGE OF zintfmonitor010-intfid,
      END   OF mtyp_ranges .

    "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
    CLASS-DATA ms_ranges TYPE mtyp_ranges .
    "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
    CLASS-DATA mt_buffer TYPE ztt_zintfmonitor010 .

    "! <p class="shorttext synchronized" lang="en">Add to Range</p>
    CLASS-METHODS _add_range
      IMPORTING
        !id_low   TYPE any
      CHANGING
        !ct_range TYPE ANY TABLE .
ENDCLASS.



CLASS zcl_zintfmonitor010_read IMPLEMENTATION.


  METHOD delete_details.
    DATA lt_list      TYPE ztt_zintfmonitor010 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO  cx_db2_not_found.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        delete_list( lt_list[] ).
      CATCH cx_db2_not_found INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_list.

    CHECK it_list[] IS NOT INITIAL.
    DELETE zintfmonitor010  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE cx_db2_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD get_detail.

    DATA lt_list      TYPE ztt_zintfmonitor010 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO cx_db2_not_found.

    READ TABLE mt_buffer INTO rs_result
      WITH KEY intfid = id_intfid.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_intfid = id_intfid
                    IMPORTING  et_list = lt_list ).

          READ TABLE lt_list INDEX 1 INTO ls_list.
          MOVE-CORRESPONDING ls_list TO rs_result.

        CATCH cx_db2_not_found INTO lo_exception.
          RAISE EXCEPTION lo_exception.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD get_detail_x.
    DATA: ls_detail TYPE zintfmonitor011.

    rs_result-detail = get_detail( id_intfid ).

* Retrieve interface name
    TRY.
        ls_detail = zcl_zintfmonitor011_read=>get_details( id_intfid = id_intfid
                                                           id_spras  = sy-langu ).

        rs_result-xintfid = ls_detail-xintfid.
      CATCH cx_db2_not_found .
    ENDTRY.

* Retrieve class name

* Retrieve domain text value


  ENDMETHOD.


  METHOD get_list.

    CLEAR ms_ranges.

    IF id_intfid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_intfid
                  CHANGING  ct_range = ms_ranges-intfid ).
    ENDIF.

    SELECT * INTO TABLE et_list
      FROM zintfmonitor010
      WHERE intfid IN ms_ranges-intfid.

    INSERT LINES OF et_list INTO TABLE mt_buffer.
    SORT mt_buffer.
    DELETE ADJACENT DUPLICATES FROM mt_buffer.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE cx_db2_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR mt_buffer[].
  ENDMETHOD.


  METHOD save_detail.
    DATA lt_list TYPE ztt_zintfmonitor010 .
    DATA ls_list LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO cx_db2_not_found.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        save_list( lt_list[] ).
      CATCH cx_db2_not_found INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD save_list.

    CHECK it_list[] IS NOT INITIAL.
    MODIFY zintfmonitor010  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE cx_db2_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD _add_range.

    FIELD-SYMBOLS: <ls_row>    TYPE any,
                   <ld_sign>   TYPE any,
                   <ld_option> TYPE any,
                   <ld_low>    TYPE any.

    INSERT INITIAL LINE INTO TABLE ct_range ASSIGNING <ls_row>.

    ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_row> TO <ld_low>.
    <ld_low> = id_low.

    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_row> TO <ld_sign>.
    <ld_sign> = 'I'.

    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_row> TO <ld_option>.
    <ld_option> = 'EQ'.
  ENDMETHOD.
ENDCLASS.
