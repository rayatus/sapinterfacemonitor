*&---------------------------------------------------------------------*
*& Report zintfmonitor_dummy_datagen
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zintfmonitor_dummy_datagen.



START-OF-SELECTION.

  DATA: ls_zintfmonitor010 TYPE zintfmonitor010,
        ls_zintfmonitor011 TYPE zintfmonitor011,
        ls_zintfmonitor012 TYPE zintfmonitor012,
        lt_zintfmonitor012 TYPE STANDARD TABLE OF zintfmonitor012.

  PERFORM prepare_custo.
  PERFORM insert_dummy_data.
  WRITE 'End'.

FORM prepare_custo.

  DELETE FROM zintfmonitor010 WHERE intfid = 'DUMMY01' OR intfid = 'DUMMY02'.
  DELETE FROM zintfmonitor011 WHERE intfid = 'DUMMY01' OR intfid = 'DUMMY02'..
  DELETE FROM zintfmonitor012 WHERE intfid = 'DUMMY01' OR intfid = 'DUMMY02'..

  "Prepare dummy custo
  ls_zintfmonitor010-intfid = 'DUMMY01'.
  ls_zintfmonitor010-inbout = '1'.
  ls_zintfmonitor010-active = abap_true.
  ls_zintfmonitor010-clsname = 'ZCL_INTFMONITOR_RFC'.
  MODIFY zintfmonitor010 FROM ls_zintfmonitor010.
  ls_zintfmonitor011-intfid  = 'DUMMY01'.
  ls_zintfmonitor011-spras   = sy-langu.
  ls_zintfmonitor011-xintfid = 'DUMMY In RFC Moni'.
  MODIFY zintfmonitor011 FROM ls_zintfmonitor011.

  ls_zintfmonitor010-intfid = 'DUMMY02'.
  ls_zintfmonitor010-inbout = '2'.
  ls_zintfmonitor010-active = abap_true.
  ls_zintfmonitor010-clsname = 'ZCL_INTFMONITOR_RFC'.
  MODIFY zintfmonitor010 FROM ls_zintfmonitor010.
  ls_zintfmonitor011-intfid  = 'DUMMY02'.
  ls_zintfmonitor011-spras   = sy-langu.
  ls_zintfmonitor011-xintfid = 'DUMMY Out RFC Moni'.
  MODIFY zintfmonitor011 FROM ls_zintfmonitor011.


  ls_zintfmonitor012-intfid     = 'DUMMY01'.
  ls_zintfmonitor012-param      = 'INPUT'.
  ls_zintfmonitor012-paramtype  = 'STRING'.
  ls_zintfmonitor012-datatype   = 'P'.
  INSERT ls_zintfmonitor012 INTO TABLE lt_zintfmonitor012.
  ls_zintfmonitor012-intfid     = 'DUMMY01'.
  ls_zintfmonitor012-param      = 'OUTPUT'.
  ls_zintfmonitor012-paramtype  = 'STRING'.
  ls_zintfmonitor012-datatype   = 'R'.
  INSERT ls_zintfmonitor012 INTO TABLE lt_zintfmonitor012.


  ls_zintfmonitor012-intfid     = 'DUMMY02'.
  ls_zintfmonitor012-param      = 'SENT'.
  ls_zintfmonitor012-paramtype  = 'STRING'.
  ls_zintfmonitor012-datatype   = 'P'.
  INSERT ls_zintfmonitor012 INTO TABLE lt_zintfmonitor012.
  ls_zintfmonitor012-intfid     = 'DUMMY02'.
  ls_zintfmonitor012-param      = 'RECEIVED'.
  ls_zintfmonitor012-paramtype  = 'STRING'.
  ls_zintfmonitor012-datatype   = 'R'.
  INSERT ls_zintfmonitor012 INTO TABLE lt_zintfmonitor012.

  MODIFY zintfmonitor012 FROM TABLE lt_zintfmonitor012.

ENDFORM.

FORM insert_dummy_data.

  DATA: begin of ls_dummy,
          dummy type c LENGTH 200,
        end   of ls_dummy.

  SELECT * FROM zintfmonitor020 INTO TABLE @DATA(lt_intfmonitor020) WHERE intfid = 'DUMMY01' OR intfid = 'DUMMY02'.
  DELETE FROM zintfmonitor020 WHERE intfid = 'DUMMY01' OR intfid = 'DUMMY02'.
  LOOP AT lt_intfmonitor020 INTO DATA(ls_intfmonitor020).
    DELETE FROM zintfmonitor021 WHERE guid = ls_intfmonitor020-guid.
  ENDLOOP.


  zcf_intfmonitor=>new(
    EXPORTING
      id_intfid                 =  'DUMMY01'
    RECEIVING
      ro_instance               = DATA(lo_instance)
    EXCEPTIONS
      not_found                 = 1
      unable_to_create_instance = 2
      OTHERS                    = 3
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ls_dummy-dummy = 'dummy input string'.
  lo_instance->add_parameter( id_param = 'INPUT'  ix_val =  ls_dummy-dummy ).
  ls_dummy-dummy = 'dummy output string'.
  lo_instance->add_parameter( id_param = 'OUTPUT' ix_val =  ls_dummy-dummy ).

  lo_instance->store( ).

  zcf_intfmonitor=>new(
  EXPORTING
    id_intfid                 =  'DUMMY02'
  RECEIVING
    ro_instance               = lo_instance
  EXCEPTIONS
    not_found                 = 1
    unable_to_create_instance = 2
    OTHERS                    = 3
).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ls_dummy-dummy = 'dummy sent string'.
  lo_instance->add_parameter( id_param = 'SENT'  ix_val =  ls_dummy-dummy ).
  ls_dummy-dummy = 'dummy received string'.
  lo_instance->add_parameter( id_param = 'RECEIVED' ix_val =  ls_dummy-dummy ).

  lo_instance->store( ).

ENDFORM.
