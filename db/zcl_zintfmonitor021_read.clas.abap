"! <p class="shorttext synchronized" lang="en">Interface executions Data</p>
CLASS zcl_zintfmonitor021_read DEFINITION
  PUBLIC
  CREATE PRIVATE INHERITING FROM zcl_zintfmonitor_base_read.

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Delete Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_details
      IMPORTING
        !is_details TYPE zintfmonitor021
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Delete Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor021
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
    "!
    "! @parameter id_guid   | <p class="shorttext synchronized" lang="en">Process Id</p>
    "! @parameter id_item   | <p class="shorttext synchronized" lang="en">Item</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    CLASS-METHODS get_details
      IMPORTING
        !id_guid         TYPE zintfmonitor021-guid
        !id_item         TYPE zintfmonitor021-item
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor021
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
    "!
    "! @parameter id_guid | <p class="shorttext synchronized" lang="en">Process Id</p>
    "! @parameter id_item | <p class="shorttext synchronized" lang="en">Item</p>
    "! @parameter et_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS get_list
      IMPORTING
        !id_guid TYPE zintfmonitor021-guid OPTIONAL
        !id_item TYPE zintfmonitor021-item OPTIONAL
      EXPORTING
        !et_list TYPE ztt_zintfmonitor021
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
    CLASS-METHODS init_buffer .
    "! <p class="shorttext synchronized" lang="en">Save Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_details
      IMPORTING
        !is_details TYPE zintfmonitor021
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Save Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor021
      RAISING
        zcx_intfmonitor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        guid TYPE RANGE OF zintfmonitor021-guid,
        item TYPE RANGE OF zintfmonitor021-item,
      END   OF mtyp_ranges .

    "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
    CLASS-DATA ms_ranges TYPE mtyp_ranges .
    "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
    CLASS-DATA mt_buffer TYPE ztt_zintfmonitor021 .

ENDCLASS.



CLASS zcl_zintfmonitor021_read IMPLEMENTATION.


  METHOD delete_details.
    DATA lt_list      TYPE ztt_zintfmonitor021 .
    DATA ls_list      LIKE LINE OF lt_list.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        delete_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO data(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_list.

    CHECK it_list[] IS NOT INITIAL.
    DELETE zintfmonitor021  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor021 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    READ TABLE mt_buffer INTO rs_result
      WITH KEY guid = id_guid
  						 item = id_item.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_guid = id_guid
  														 id_item = id_item
                    IMPORTING  et_list = lt_list ).

          READ TABLE lt_list INDEX 1 INTO ls_list.
          MOVE-CORRESPONDING ls_list TO rs_result.

        CATCH zcx_intfmonitor INTO lo_exception.
          RAISE EXCEPTION lo_exception.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD get_list.

    CLEAR ms_ranges.

    IF id_guid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_guid
                  CHANGING  ct_range = ms_ranges-guid ).
    ENDIF.

    IF id_item IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_item
                  CHANGING  ct_range = ms_ranges-item ).
    ENDIF.

    SELECT * INTO TABLE et_list
      FROM zintfmonitor021
      WHERE guid IN ms_ranges-guid
        AND item IN ms_ranges-item.

    INSERT LINES OF et_list INTO TABLE mt_buffer.
    SORT mt_buffer.
    DELETE ADJACENT DUPLICATES FROM mt_buffer.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR mt_buffer[].
  ENDMETHOD.


  METHOD save_details.
    DATA lt_list TYPE ztt_zintfmonitor021 .
    DATA ls_list LIKE LINE OF lt_list.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        save_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO data(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD save_list.

    CHECK it_list[] IS NOT INITIAL.
    MODIFY zintfmonitor021  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
