class ZCL_ZINTFMONITOR021_READ definition
  public
  create private .

public section.

  class-methods DELETE_DETAILS
    importing
      !IS_DETAILS type ZINTFMONITOR021
    raising
      zcx_intfmonitor .
  class-methods DELETE_LIST
    importing
      !IT_LIST type ZTT_ZINTFMONITOR021
    raising
      zcx_intfmonitor .
  class-methods GET_DETAILS
    importing
      !ID_GUID type ZINTFMONITOR021-GUID
      !ID_ITEM type ZINTFMONITOR021-ITEM
    returning
      value(RS_RESULT) type ZINTFMONITOR021
    raising
      zcx_intfmonitor .
  class-methods GET_LIST
    importing
      !ID_GUID type ZINTFMONITOR021-GUID optional
      !ID_ITEM type ZINTFMONITOR021-ITEM optional
    exporting
      !ET_LIST type ZTT_ZINTFMONITOR021
    raising
      zcx_intfmonitor .
  class-methods INIT_BUFFER .
  class-methods SAVE_DETAILS
    importing
      !IS_DETAILS type ZINTFMONITOR021
    raising
      zcx_intfmonitor .
  class-methods SAVE_LIST
    importing
      !IT_LIST type ZTT_ZINTFMONITOR021
    raising
      zcx_intfmonitor .
protected section.
private section.

  types:
     BEGIN OF mtyp_ranges,
 GUID TYPE RANGE OF ZINTFMONITOR021-GUID ,
 ITEM TYPE RANGE OF ZINTFMONITOR021-ITEM ,
        END   OF mtyp_ranges .

  class-data MS_RANGES type MTYP_RANGES .
  class-data MT_BUFFER type ZTT_ZINTFMONITOR021 .

  class-methods _ADD_RANGE
    importing
      !ID_LOW type ANY
    changing
      !CT_RANGE type ANY TABLE .
ENDCLASS.



CLASS ZCL_ZINTFMONITOR021_READ IMPLEMENTATION.


  method DELETE_DETAILS.
  DATA lt_list      TYPE ZTT_ZINTFMONITOR021 .
  DATA ls_list      LIKE LINE OF lt_list.
  DATA lo_exception TYPE REF TO  zcx_intfmonitor.

  CHECK is_details IS NOT INITIAL.
  TRY .
      MOVE-CORRESPONDING is_details TO ls_list.
      INSERT ls_list INTO TABLE lt_list[].
      delete_list( lt_list[] ).
    CATCH zcx_intfmonitor INTO lo_exception.
      RAISE EXCEPTION lo_exception.
  ENDTRY.
  endmethod.


  method DELETE_LIST.

  CHECK it_list[] IS NOT INITIAL.
  DELETE ZINTFMONITOR021  FROM TABLE it_list.

  IF NOT sy-subrc IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.


  method GET_DETAILS.

  DATA lt_list      TYPE ZTT_ZINTFMONITOR021 .
  DATA ls_list      LIKE LINE OF lt_list.
  DATA lo_exception TYPE REF TO zcx_intfmonitor.

  READ TABLE mt_buffer INTO rs_result
    WITH KEY GUID = ID_GUID
						 ITEM = ID_ITEM.

  IF NOT sy-subrc IS INITIAL.

    TRY.
        get_list( EXPORTING  ID_GUID = ID_GUID
														 ID_ITEM = ID_ITEM
                  IMPORTING  et_list = lt_list ).

      READ TABLE lt_list INDEX 1 INTO ls_list.
      MOVE-CORRESPONDING ls_list TO rs_result.

      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.

  ENDIF.
  endmethod.


  method GET_LIST.

  CLEAR ms_ranges.

  IF ID_GUID IS SUPPLIED.
    _add_range( EXPORTING id_low   = ID_GUID
                CHANGING  ct_range = ms_ranges-GUID ).
  ENDIF.

  IF ID_ITEM IS SUPPLIED.
    _add_range( EXPORTING id_low   = ID_ITEM
                CHANGING  ct_range = ms_ranges-ITEM ).
  ENDIF.

SELECT * INTO TABLE et_list
  FROM ZINTFMONITOR021
  WHERE GUID IN ms_ranges-GUID
    AND ITEM IN ms_ranges-ITEM.

  INSERT LINES OF et_list INTO TABLE mt_buffer.
  SORT mt_buffer.
  DELETE ADJACENT DUPLICATES FROM mt_buffer.

  IF et_list IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.


  method INIT_BUFFER.
  CLEAR mt_buffer[].
  endmethod.


  method SAVE_DETAILS.
  DATA lt_list type ZTT_ZINTFMONITOR021 .
  DATA ls_list like line of lt_list.
  DATA lo_exception TYPE REF TO zcx_intfmonitor.

  CHECK is_details IS NOT INITIAL.
  TRY .
      MOVE-CORRESPONDING is_details TO ls_list.
      INSERT ls_list INTO TABLE lt_list[].
      save_list( lt_list[] ).
    CATCH zcx_intfmonitor INTO lo_exception.
      RAISE EXCEPTION lo_exception.
  ENDTRY.
  endmethod.


  method SAVE_LIST.

  CHECK it_list[] IS NOT INITIAL.
  MODIFY ZINTFMONITOR021  FROM TABLE it_list.

  IF NOT sy-subrc IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.


  method _ADD_RANGE.

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
  endmethod.
ENDCLASS.
