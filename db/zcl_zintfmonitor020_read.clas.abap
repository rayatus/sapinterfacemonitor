"! <p class="shorttext synchronized" lang="en">Interface executions</p>
CLASS zcl_zintfmonitor020_read DEFINITION
  PUBLIC
  CREATE PRIVATE INHERITING FROM zcl_zintfmonitor_base_read.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF mtyp_all_fields,
        intfid      TYPE RANGE OF zzeintfid,
        procdate    TYPE RANGE OF zzeprocdate,
        proctime    TYPE RANGE OF zzeproctime,
        procby      TYPE RANGE OF zzeprocby,
        procendtype TYPE RANGE OF zzeprocendtype,
        lognumber   TYPE RANGE OF balognr,
      END OF mtyp_all_fields.

    "! <p class="shorttext synchronized" lang="en">Delete Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_details
      IMPORTING
        !is_details TYPE zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Delete Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
    "!
    "! @parameter id_guid   | <p class="shorttext synchronized" lang="en">Process Id</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    CLASS-METHODS get_details
      IMPORTING
        !id_guid         TYPE zintfmonitor020-guid
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
    "!
    "! @parameter id_guid | <p class="shorttext synchronized" lang="en">Process Id</p>
    "! @parameter et_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS get_list
      IMPORTING
        !id_guid TYPE zintfmonitor020-guid OPTIONAL
      EXPORTING
        !et_list TYPE ztt_zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
    CLASS-METHODS init_buffer .
    "! <p class="shorttext synchronized" lang="en">Save Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_details
      IMPORTING
        !is_details TYPE zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Save Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor020
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS get_list_by
      IMPORTING
        is_filter_by TYPE mtyp_all_fields
      EXPORTING
        et_list      TYPE ztt_zintfmonitor020 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        guid TYPE RANGE OF zintfmonitor020-guid,
      END   OF mtyp_ranges .

    "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
    CLASS-DATA gs_ranges TYPE mtyp_ranges .
    "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
    CLASS-DATA gt_buffer TYPE ztt_zintfmonitor020 .

ENDCLASS.



CLASS zcl_zintfmonitor020_read IMPLEMENTATION.


  METHOD delete_details.
    DATA lt_list      TYPE ztt_zintfmonitor020 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO  zcx_intfmonitor.

    ASSERT is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        delete_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_list.

    ASSERT it_list[] IS NOT INITIAL.
    DELETE zintfmonitor020  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor020 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    READ TABLE gt_buffer INTO rs_result
      WITH KEY guid = id_guid.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_guid = id_guid
                    IMPORTING  et_list = lt_list ).

          READ TABLE lt_list INDEX 1 INTO ls_list.
          MOVE-CORRESPONDING ls_list TO rs_result.

        CATCH zcx_intfmonitor INTO lo_exception.
          RAISE EXCEPTION lo_exception.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD get_list.

    CLEAR gs_ranges.

    IF id_guid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_guid
                  CHANGING  ct_range = gs_ranges-guid ).
    ENDIF.

    SELECT * INTO TABLE et_list
      FROM zintfmonitor020
      WHERE guid IN gs_ranges-guid.

    INSERT LINES OF et_list INTO TABLE gt_buffer.
    SORT gt_buffer.
    DELETE ADJACENT DUPLICATES FROM gt_buffer.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR gt_buffer[].
  ENDMETHOD.


  METHOD save_details.
    DATA lt_list TYPE ztt_zintfmonitor020 .
    DATA ls_list LIKE LINE OF lt_list.

    ASSERT is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        save_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD save_list.

    ASSERT it_list[] IS NOT INITIAL.
    MODIFY zintfmonitor020  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD get_list_by.

    SELECT * FROM zintfmonitor020
     INTO TABLE et_list
      WHERE intfid        IN is_filter_by-intfid
         AND procdate     IN is_filter_by-procdate
         AND proctime     IN is_filter_by-proctime
         AND procby       IN is_filter_by-procby
         AND procendtype  IN is_filter_by-procendtype
         AND lognumber    IN is_filter_by-lognumber.


  ENDMETHOD.
ENDCLASS.
