"! <p class="shorttext synchronized" lang="en">SAP Monitor Interface Appl - Gui Component</p>
CLASS zcl_intfmonitor_gui_appl_comp DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">List of interface executions</p>
    DATA mt_list TYPE ztt_zintfmonitor030 READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Sets data list in order to be shown on screen</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List of interface executions</p>
    METHODS set_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor030
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Displays data on screen</p>
    "!
    "! @parameter io_container | <p class="shorttext synchronized" lang="en">Abstract Container for GUI Controls</p>
    METHODS display
      IMPORTING
        !io_container TYPE REF TO cl_gui_container .
  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">Abstract Container for GUI Controls</p>
    DATA mo_container TYPE REF TO cl_gui_container .

    "! <p class="shorttext synchronized" lang="en">Prepares data to be displayed on screen</p>
    METHODS prepare_data ABSTRACT
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Own Display implementation</p>
    METHODS _display
        ABSTRACT .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTFMONITOR_GUI_APPL_COMP IMPLEMENTATION.


  METHOD display.
    mo_container = io_container.
    _display( ).
  ENDMETHOD.


  METHOD set_list.

    me->mt_list[] = it_list[].
    prepare_data( ).

  ENDMETHOD.
ENDCLASS.
