"! <p class="shorttext synchronized" lang="en">Interfaces</p>
CLASS zcl_zintfmonitor010_read DEFINITION
  PUBLIC
  CREATE PRIVATE INHERITING FROM zcl_zintfmonitor_base_read.

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Class constructor</p>
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    "! @raising zcx_intfmonitor | <p class="shorttext synchronized" lang="en">Unexpected error</p>
    CLASS-METHODS get_details
      IMPORTING id_intfid        TYPE zintfmonitor010-intfid
      RETURNING VALUE(rs_result) TYPE zintfmonitor010
      RAISING   zcx_intfmonitor .

    "! <p class="shorttext synchronized" lang="en">Find details by keys returning also descriptions</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter id_langu |  <p class="shorttext synchronized" lang="en">Language for descriptions</p>
    "! @parameter rs_detail_x | <p class="shorttext synchronized" lang="en">Detail information with descriptions</p>
    "! @raising zcx_intfmonitor | <p class="shorttext synchronized" lang="en">Unexpected error</p>
    CLASS-METHODS get_detail_x
      IMPORTING id_intfid          TYPE zintfmonitor010-intfid
                id_langu           TYPE sy-langu DEFAULT sy-langu
      RETURNING VALUE(rs_detail_x) TYPE zeintfmonitor010_detail_x
      RAISING   zcx_intfmonitor .

    "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter et_list   | <p class="shorttext synchronized" lang="en">List Details</p>
    "! @raising zcx_intfmonitor | <p class="shorttext synchronized" lang="en">Unexpected error</p>
    CLASS-METHODS get_list
      IMPORTING id_intfid TYPE zintfmonitor010-intfid OPTIONAL
      EXPORTING et_list   TYPE ztt_zintfmonitor010
      RAISING   zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
    CLASS-METHODS init_buffer .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        intfid TYPE RANGE OF zintfmonitor010-intfid,
      END   OF mtyp_ranges .

    "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
    CLASS-DATA gs_ranges TYPE mtyp_ranges .
    "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
    CLASS-DATA gt_buffer TYPE ztt_zintfmonitor010 .
    "! <p class="shorttext synchronized" lang="en">Reads DB Table</p>
    CLASS-METHODS do_select.


ENDCLASS.



CLASS zcl_zintfmonitor010_read IMPLEMENTATION.

  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor010 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    READ TABLE gt_buffer INTO rs_result
      WITH KEY intfid = id_intfid.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_intfid = id_intfid
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

    IF id_intfid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_intfid
                  CHANGING  ct_range = gs_ranges-intfid ).
    ENDIF.

    LOOP AT gt_buffer INTO DATA(ls_buffer) WHERE intfid IN gs_ranges-intfid.
      INSERT INITIAL LINE INTO TABLE et_list ASSIGNING FIELD-SYMBOL(<ls_list>).
      MOVE-CORRESPONDING ls_buffer TO <ls_list>.
    ENDLOOP.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR gt_buffer[].
    do_select( ).
  ENDMETHOD.


  METHOD get_detail_x.
    TRY.
        DATA(ls_detail) = get_details( id_intfid = id_intfid ).

        MOVE-CORRESPONDING ls_detail TO rs_detail_x.

        SELECT SINGLE descript
          INTO rs_detail_x-xclsname
          FROM seoclasstx
          WHERE clsname = rs_detail_x-clsname
            AND langu   = id_langu.
        IF sy-subrc IS NOT INITIAL.
          rs_detail_x-xclsname = |< { rs_detail_x-xclsname } >|.
        ENDIF.

        rs_detail_x-xinbout = zcl_intfmonitor_util=>get_domain_text( id_domname = 'ZZDINTFINOUT'
                                                                     id_value   = rs_detail_x-inbout ).

        TRY.
            DATA(ls_intfmonitor011) = zcl_zintfmonitor011_read=>get_details( id_intfid = rs_detail_x-intfid ).
            rs_detail_x-xintfid = ls_intfmonitor011-xintfid.
          CATCH zcx_intfmonitor.
            rs_detail_x-xintfid = |< { rs_detail_x-intfid } >|.
        ENDTRY.

      CATCH zcx_intfmonitor INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD do_select.

    SELECT * INTO TABLE gt_buffer FROM zintfmonitor010 ORDER BY PRIMARY KEY.

  ENDMETHOD.

  METHOD class_constructor.
    init_buffer( ).
  ENDMETHOD.

ENDCLASS.
