*&---------------------------------------------------------------------*
*& Report ZUML_CLASS_DIAGRAM
*& UML Class Diagram
*&---------------------------------------------------------------------*
*& Developer: admin@abap4.ru
*&---------------------------------------------------------------------*
REPORT zuml_class_diagram.

CLASS zcl_uml_class_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES uml_string_tab TYPE cl_uml_class_scanner=>uml_string_tab.
    TYPES uml_line TYPE cl_uml_class_scanner=>uml_line.

    METHODS constructor
      IMPORTING is_uml_line TYPE uml_line
                iv_abs_type TYPE string.
    CLASS-METHODS factory
      IMPORTING
        is_uml_line     TYPE uml_line
      RETURNING
        VALUE(ro_class) TYPE REF TO zcl_uml_class_base.
    METHODS build_plant_uml
      RETURNING VALUE(rt_value) TYPE uml_string_tab.
    METHODS get_adir_key
      RETURNING VALUE(rs_adir_key) TYPE adir_key.
    METHODS is_maintenance_view
      RETURNING VALUE(rv_value) TYPE flag.
    METHODS set_visibility
      IMPORTING iv_private   TYPE flag
                iv_protected TYPE flag
                iv_public    TYPE flag.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF mts_class_item,
        is_abstract TYPE c LENGTH 1,
        is_static   TYPE c LENGTH 1,
        visibility  TYPE c LENGTH 1,
        name        TYPE string,
      END OF mts_class_item.

    METHODS get_name
      IMPORTING iv_escape       TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_value) TYPE string.
    METHODS get_container
      RETURNING VALUE(rv_value) TYPE string.
    METHODS get_class_header
      RETURNING VALUE(rv_value) TYPE string.
    METHODS get_class_item
      IMPORTING is_class_item   TYPE mts_class_item
      RETURNING VALUE(rv_value) TYPE string.
    METHODS build_relation
      IMPORTING it_data         TYPE STANDARD TABLE
                iv_sep          TYPE clike
                iv_back         TYPE flag DEFAULT space
                iv_comment      TYPE clike OPTIONAL
      RETURNING VALUE(rt_value) TYPE uml_string_tab.
    METHODS build_attributes
      RETURNING VALUE(rt_value) TYPE uml_string_tab.
    METHODS build_methods
      RETURNING VALUE(rt_value) TYPE uml_string_tab.
    METHODS build_events
      RETURNING VALUE(rt_value) TYPE uml_string_tab.
    DATA mv_visibility TYPE string.
    DATA mv_abs_type TYPE string.
    DATA ms_uml_line TYPE uml_line.
ENDCLASS.

CLASS zcl_uml_class_class DEFINITION INHERITING FROM zcl_uml_class_base.
  PROTECTED SECTION.
    METHODS get_class_header REDEFINITION.
ENDCLASS.

CLASS zcl_uml_class_interface DEFINITION INHERITING FROM zcl_uml_class_base.
  PROTECTED SECTION.
    METHODS get_class_header REDEFINITION.
ENDCLASS.

CLASS zcl_uml_class_program DEFINITION INHERITING FROM zcl_uml_class_base.
  PROTECTED SECTION.
    METHODS get_class_header REDEFINITION.
ENDCLASS.

CLASS zcl_uml_class_function_group DEFINITION INHERITING FROM zcl_uml_class_base.
  PUBLIC SECTION.
    METHODS is_maintenance_view REDEFINITION.
  PROTECTED SECTION.
    METHODS get_class_header REDEFINITION.
ENDCLASS.

CLASS zcl_uml_class_base IMPLEMENTATION.
  METHOD constructor.
    ms_uml_line = is_uml_line.
    mv_abs_type = iv_abs_type.
    set_visibility(
        iv_private   = abap_true
        iv_protected = abap_true
        iv_public    = abap_true
    ).
  ENDMETHOD.

  METHOD factory.
    DATA(lv_last) = match( val = is_uml_line-name regex = '\\[^\\]+\s*$' ).
    DATA(lv_type) = substring_before( val = lv_last sub = '=' ).
    CASE lv_type.
      WHEN cl_uml_class_scanner=>c_t_class.
        ro_class ?= NEW zcl_uml_class_class( is_uml_line = is_uml_line iv_abs_type = lv_type ).
      WHEN cl_uml_class_scanner=>c_t_interface.
        ro_class ?= NEW zcl_uml_class_interface( is_uml_line = is_uml_line iv_abs_type = lv_type ).
      WHEN cl_uml_class_scanner=>c_t_program.
        ro_class ?= NEW zcl_uml_class_program( is_uml_line = is_uml_line iv_abs_type = lv_type ).
      WHEN cl_uml_class_scanner=>c_t_fugr.
        ro_class ?= NEW zcl_uml_class_function_group( is_uml_line = is_uml_line iv_abs_type = lv_type ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_name.
    DATA(lv_name) = substring_after( val = ms_uml_line-name sub = mv_abs_type && '=' ).
    IF iv_escape EQ abap_true AND lv_name CA '\=/'.
      lv_name = |"{ lv_name }"|.
    ENDIF.
    DATA(lv_container) = get_container( ).
    IF lv_container IS NOT INITIAL.
      rv_value = |{ lv_container }::{ lv_name }|.
    ELSE.
      rv_value = |{ lv_name }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_adir_key.
    DATA(lv_first) = match( val = ms_uml_line-name regex = '\\\w+=[^\\]+' ).
    DATA(lv_type) = substring_before( val = lv_first sub = '=' ).
    DATA(lv_name) = substring_after( val = lv_first sub = '=' ).
    CASE lv_type.
      WHEN cl_uml_class_scanner=>c_t_class OR cl_uml_class_scanner=>c_t_cpool.
        rs_adir_key = VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = lv_name ).
      WHEN cl_uml_class_scanner=>c_t_interface.
        rs_adir_key = VALUE #( pgmid = 'R3TR' object = 'INTF' obj_name = lv_name ).
      WHEN cl_uml_class_scanner=>c_t_program.
        rs_adir_key = VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = lv_name ).
      WHEN cl_uml_class_scanner=>c_t_fugr OR cl_uml_class_scanner=>c_t_fpool.
        rs_adir_key = VALUE #( pgmid = 'R3TR' object = 'FUGR' obj_name = lv_name ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_visibility.
    CLEAR mv_visibility.
    IF iv_private EQ abap_true.
      mv_visibility = mv_visibility && 'AI'.
    ENDIF.
    IF iv_protected EQ abap_true.
      mv_visibility = mv_visibility && 'O'.
    ENDIF.
    IF iv_public EQ abap_true.
      mv_visibility = mv_visibility && 'U'.
    ENDIF.
  ENDMETHOD.

  METHOD get_container.
    DATA(lv_first) = match( val = ms_uml_line-name regex = '^\\\w+=[^\\]+' ).
    DATA(lv_last) = match( val = ms_uml_line-name regex = '\\\w+=[^\\]+\s*$' ).
    DATA(ls_adir_key) = get_adir_key( ).
    IF lv_first EQ lv_last AND |CLAS/INTF| CS ls_adir_key-object.
      rv_value = ||.
    ELSE.
      rv_value = |{ ls_adir_key-object }::{ ls_adir_key-obj_name }|.
    ENDIF.
  ENDMETHOD.

  METHOD is_maintenance_view.
    rv_value = abap_false.
  ENDMETHOD.

  METHOD build_plant_uml.
    DATA lt_uml TYPE TABLE OF string.
    DATA lt_relation TYPE TABLE OF string.

    IF ms_uml_line-supertype IS NOT INITIAL.
      DATA(lo_parent) = zcl_uml_class_base=>factory( VALUE #( name = ms_uml_line-supertype ) ).
      IF lo_parent IS BOUND.
        lt_relation = build_relation( it_data = VALUE uml_string_tab( ( CONV #( ms_uml_line-supertype ) ) ) iv_sep = '<|--' iv_back = abap_true ).
      ENDIF.
    ENDIF.
    APPEND LINES OF build_relation( it_data = ms_uml_line-t_implementations iv_sep = '<|..' iv_back = abap_true ) TO lt_relation.
    APPEND LINES OF build_relation( it_data = ms_uml_line-t_user iv_sep = '<--' ) TO lt_relation.
    APPEND LINES OF build_relation( it_data = ms_uml_line-t_exceptions iv_sep = '..>' iv_comment = | : exception | ) TO lt_relation.
    APPEND LINES OF build_relation( it_data = ms_uml_line-t_friends iv_sep = '..>' iv_comment = | : friend | ) TO lt_relation.

    lt_uml = VALUE #( BASE lt_uml ( |{ get_class_header( ) } { '{' }| ) ).
    lt_uml = VALUE #( BASE lt_uml FOR wa IN build_attributes( ) WHERE ( table_line NE || ) ( wa ) ).
    lt_uml = VALUE #( BASE lt_uml FOR wa IN build_methods( ) WHERE ( table_line NE || ) ( wa ) ).
    lt_uml = VALUE #( BASE lt_uml FOR wa IN build_events( ) WHERE ( table_line NE || ) ( wa ) ).
    lt_uml = VALUE #( BASE lt_uml ( |{ '}' }| ) ).
    lt_uml = VALUE #( BASE lt_uml FOR wa IN lt_relation ( wa ) ).
    rt_value[] = lt_uml[].
  ENDMETHOD.

  METHOD get_class_header.
    rv_value = |annotation { get_name( ) }|.
  ENDMETHOD.

  METHOD get_class_item.
    rv_value = ||.
    CHECK mv_visibility CS is_class_item-visibility.
    IF is_class_item-is_abstract EQ abap_true.
      rv_value = |{ rv_value }{ '{abstract}' } |.
    ENDIF.
    CASE is_class_item-visibility.
      WHEN 'A'. rv_value = |{ rv_value }~ |.
      WHEN 'I'. rv_value = |{ rv_value }- |.
      WHEN 'O'. rv_value = |{ rv_value }# |.
      WHEN 'U'. rv_value = |{ rv_value }+ |.
    ENDCASE.
    IF is_class_item-is_static EQ abap_true.
      rv_value = |{ rv_value }{ '{static}' } |.
    ENDIF.
    rv_value = |{ rv_value }{ is_class_item-name } |.
  ENDMETHOD.

  METHOD build_relation.
    DATA(lv_prefix) = ||.
    DATA(lv_suffix) = ||.
    IF iv_back EQ space.
      lv_prefix = |{ get_name( ) } { iv_sep } |.
    ELSE.
      lv_suffix = | { iv_sep } { get_name( ) }|.
    ENDIF.
    IF iv_comment IS NOT INITIAL.
      lv_suffix = |{ lv_suffix } { iv_comment }|.
    ENDIF.
    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<lv_name>).
      DATA(lo_class) = zcl_uml_class_base=>factory( VALUE #( name = <lv_name> ) ).
      IF lo_class IS BOUND.
        rt_value = VALUE #( BASE rt_value ( |{ lv_prefix }{ lo_class->get_name( ) }{ lv_suffix }| ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_attributes.
    SORT ms_uml_line-t_attributes BY visibility name.
    LOOP AT ms_uml_line-t_attributes ASSIGNING FIELD-SYMBOL(<ls>).
      DATA(ls_item) = CORRESPONDING mts_class_item( <ls> ).
      ls_item-name = |{ ls_item-name }|.
      rt_value = VALUE #( BASE rt_value ( get_class_item( ls_item ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD build_methods.
    SORT ms_uml_line-t_methods BY visibility name.
    LOOP AT ms_uml_line-t_methods ASSIGNING FIELD-SYMBOL(<ls>).
      DATA(ls_item) = CORRESPONDING mts_class_item( <ls> ).
      ls_item-name = |{ ls_item-name }()|.
      rt_value = VALUE #( BASE rt_value ( get_class_item( ls_item ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD build_events.
    SORT ms_uml_line-t_events BY visibility name.
    LOOP AT ms_uml_line-t_events ASSIGNING FIELD-SYMBOL(<ls>).
      DATA(ls_item) = CORRESPONDING mts_class_item( <ls> ).
      ls_item-name = |{ ls_item-name }()|.
      rt_value = VALUE #( BASE rt_value ( get_class_item( ls_item ) ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_uml_class_class IMPLEMENTATION.
  METHOD get_class_header.
    IF ms_uml_line-is_abstract EQ abap_true.
      rv_value = |abstract class { get_name( ) }|.
    ELSE.
      rv_value = |class { get_name( ) }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_uml_class_interface IMPLEMENTATION.
  METHOD get_class_header.
    rv_value = |interface { get_name( ) }|.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_uml_class_program IMPLEMENTATION.
  METHOD get_class_header.
    rv_value = |class { get_name( ) } <<(P, #00FFEE) >>|.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_uml_class_function_group IMPLEMENTATION.
  METHOD get_class_header.
    rv_value = |class { get_name( ) } <<(F, #FF7700) >>|.
  ENDMETHOD.

  METHOD is_maintenance_view.
    DATA lv_count TYPE i.

    LOOP AT ms_uml_line-t_methods ASSIGNING FIELD-SYMBOL(<ls>).
      CHECK match( val = <ls>-name regex = '^(TABLE|VIEW)(PROC|FRAME)_' ) NE ||.
      ADD 1 TO lv_count.
    ENDLOOP.
    rv_value = xsdbool( lv_count GE 2 AND lv_count EQ lines( ms_uml_line-t_methods ) ).
  ENDMETHOD.
ENDCLASS.

CLASS zcl_uml_class_scanner DEFINITION
  INHERITING FROM cl_uml_class_scanner.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF mts_plant_uml_attributes,
        display_packages          TYPE RANGE OF tadir-devclass,
        display_types             TYPE RANGE OF tadir-obj_name,
        display_attributes        TYPE flag,
        display_methods           TYPE flag,
        display_events            TYPE flag,
        display_friends           TYPE flag,
        display_exceptions        TYPE flag,
        exclude_maintenance_views TYPE flag,
        display_public            TYPE flag,
        display_protected         TYPE flag,
        display_private           TYPE flag,
      END OF mts_plant_uml_attributes .

    METHODS build_plant_uml
      IMPORTING
        !is_attrs       TYPE mts_plant_uml_attributes
      RETURNING
        VALUE(rv_value) TYPE string
      RAISING
        cx_dynamic_check .

  PRIVATE SECTION.
    METHODS retrieve_diagram
      IMPORTING
                !is_attrs     TYPE mts_plant_uml_attributes
      RETURNING VALUE(rt_uml) TYPE uml_tab.
ENDCLASS.

CLASS zcl_uml_class_scanner IMPLEMENTATION.
  METHOD retrieve_diagram.
    TYPES:
      BEGIN OF lts_object,
        name TYPE abap_abstypename,
        adir TYPE adir_key,
        skip TYPE flag,
      END OF lts_object.
    DATA lo_tab TYPE REF TO data.
    DATA lo_class TYPE REF TO zcl_uml_class_base.
    DATA lht_adir_key TYPE HASHED TABLE OF adir_key WITH UNIQUE KEY table_line.
    DATA lht_objects TYPE HASHED TABLE OF lts_object WITH UNIQUE KEY name.
    FIELD-SYMBOLS <lt_tab> TYPE uml_tab.
    FIELD-SYMBOLS <lv_name> TYPE clike.
    FIELD-SYMBOLS <lt_names> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_object> TYPE lts_object.
    FIELD-SYMBOLS <ls_uml> TYPE uml_line.

    get_diagram( CHANGING c_data = lo_tab ).
    ASSIGN lo_tab->* TO <lt_tab>.
    ASSERT sy-subrc EQ 0.
    SORT <lt_tab> BY name.
    MOVE <lt_tab>[] TO rt_uml[].

    DELETE rt_uml WHERE kind NA 'OFP'. " Object/Function/Program
    IF is_attrs-display_exceptions IS INITIAL.
      DELETE rt_uml WHERE is_exception EQ abap_true.
    ENDIF.
    LOOP AT rt_uml ASSIGNING <ls_uml>.
      DATA(lt_names) = VALUE uml_string_tab( ( |{ <ls_uml>-name }| ) ( |{ <ls_uml>-supertype }| ) ).
      APPEND LINES OF <ls_uml>-t_implementations TO lt_names.
      APPEND LINES OF <ls_uml>-t_user TO lt_names.
      APPEND LINES OF <ls_uml>-t_exceptions TO lt_names.
      APPEND LINES OF <ls_uml>-t_friends TO lt_names.
      LOOP AT lt_names ASSIGNING <lv_name>.
        CHECK NOT line_exists( lht_objects[ name = <lv_name> ] ).
        INSERT VALUE #( name = <lv_name> ) INTO TABLE lht_objects.
      ENDLOOP.
    ENDLOOP.
    CHECK lht_objects[] IS NOT INITIAL.

    LOOP AT lht_objects ASSIGNING <ls_object>.
      lo_class = zcl_uml_class_base=>factory( VALUE #( name = <ls_object>-name ) ).
      IF lo_class IS BOUND.
        <ls_object>-adir = lo_class->get_adir_key( ).
      ENDIF.
    ENDLOOP.

    SELECT pgmid, object, obj_name
      INTO TABLE @lht_adir_key
      FROM tadir
      FOR ALL ENTRIES IN @lht_objects
      WHERE pgmid EQ @lht_objects-adir-pgmid
        AND object EQ @lht_objects-adir-object
        AND obj_name EQ @lht_objects-adir-obj_name
        AND devclass IN @is_attrs-display_packages
        AND obj_name IN @is_attrs-display_types.
    LOOP AT lht_objects ASSIGNING <ls_object>.
      <ls_object>-skip = boolc( NOT line_exists( lht_adir_key[ table_line = <ls_object>-adir ] ) ).
    ENDLOOP.
    DELETE lht_objects WHERE skip EQ abap_true.

    LOOP AT rt_uml ASSIGNING <ls_uml>.
      lo_class = zcl_uml_class_base=>factory( <ls_uml> ).
      IF NOT line_exists( lht_objects[ name = <ls_uml>-name ] ) OR
        is_attrs-exclude_maintenance_views EQ abap_true AND lo_class->is_maintenance_view( ) EQ abap_true.
        DELETE rt_uml.
        CONTINUE.
      ENDIF.
      IF NOT line_exists( lht_objects[ name = <ls_uml>-supertype ] ).
        CLEAR <ls_uml>-supertype.
      ENDIF.
      IF is_attrs-display_attributes IS INITIAL.
        CLEAR <ls_uml>-t_attributes.
      ENDIF.
      IF is_attrs-display_methods IS INITIAL.
        CLEAR <ls_uml>-t_methods.
      ENDIF.
      IF is_attrs-display_events IS INITIAL.
        CLEAR <ls_uml>-t_events.
      ENDIF.
      IF is_attrs-display_friends IS INITIAL.
        CLEAR <ls_uml>-t_friends.
      ENDIF.
      IF is_attrs-display_exceptions IS INITIAL.
        CLEAR <ls_uml>-t_exceptions.
      ENDIF.
      DO 4 TIMES.
        CASE sy-index.
          WHEN 1. ASSIGN <ls_uml>-t_implementations TO <lt_names>.
          WHEN 2. ASSIGN <ls_uml>-t_user TO <lt_names>.
          WHEN 3. ASSIGN <ls_uml>-t_exceptions TO <lt_names>.
          WHEN 4. ASSIGN <ls_uml>-t_friends TO <lt_names>.
        ENDCASE.
        LOOP AT <lt_names> ASSIGNING <lv_name>.
          IF NOT line_exists( lht_objects[ name = <lv_name> ] ).
            DELETE <lt_names>.
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_plant_uml.
    DATA lt_uml TYPE TABLE OF string.
    DATA(lt_uml_tab) = retrieve_diagram( is_attrs ).

    lt_uml = VALUE #( BASE lt_uml ( |@startuml| ) ).
    lt_uml = VALUE #( BASE lt_uml ( |skinparam shadowing false| ) ).
    lt_uml = VALUE #( BASE lt_uml ( |set namespaceSeparator ::| ) ).
    LOOP AT lt_uml_tab ASSIGNING FIELD-SYMBOL(<lt_uml>)
      GROUP BY <lt_uml>-container.
      LOOP AT GROUP <lt_uml> ASSIGNING FIELD-SYMBOL(<ls_uml>).
        DATA(lo_class) = zcl_uml_class_base=>factory( <ls_uml> ).
        CALL METHOD lo_class->set_visibility
          EXPORTING
            iv_private   = is_attrs-display_private
            iv_protected = is_attrs-display_protected
            iv_public    = is_attrs-display_public.
        APPEND LINES OF lo_class->build_plant_uml( ) TO lt_uml.
        APPEND space TO lt_uml.
      ENDLOOP.
    ENDLOOP.
    lt_uml = VALUE #( BASE lt_uml ( |@enduml| ) ).
    rv_value = concat_lines_of( table = lt_uml sep = cl_abap_char_utilities=>cr_lf ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_app DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF mts_criteria.
        INCLUDE TYPE zcl_uml_class_scanner=>mts_plant_uml_attributes.
      TYPES:
        scan_local_types     TYPE flag,
        scan_programs        TYPE flag,
        scan_function_groups TYPE flag,
        add_structures       TYPE flag,
        scan_deep            TYPE flag,
        scan_used_types      TYPE flag,
        scan_packages        TYPE RANGE OF tadir-devclass,
        scan_types           TYPE RANGE OF tadir-obj_name,
      END OF mts_criteria.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_app.

    METHODS set_criteria
      IMPORTING is_criteria        TYPE mts_criteria
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_app.

    METHODS execute.
    METHODS open_browser.

  PRIVATE SECTION.
    CONSTANTS mc_base_url TYPE string VALUE 'http://abap4.ru/plantuml/'.
    CLASS-DATA mo_instance TYPE REF TO lcl_app.
    DATA mv_uml_text TYPE string.
    DATA ms_criteria TYPE mts_criteria.
    DATA mo_docking TYPE REF TO cl_gui_docking_container.
    DATA mo_html_viewer TYPE REF TO cl_gui_html_viewer .

    METHODS constructor.
    METHODS display_uml.
    METHODS get_encoded_uml
      RETURNING VALUE(rv_url) TYPE string.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    DATA lv_width TYPE i.

    mo_docking = NEW #( side = cl_gui_docking_container=>dock_at_right ratio = 50 ).
    mo_html_viewer = NEW #( parent = mo_docking ).
    mo_docking->get_width( IMPORTING width = lv_width ).
    cl_gui_cfw=>flush( ).
    lv_width = lv_width * 2 - cl_gui_cfw=>compute_metric_from_dynp( x_or_y = 'X' in = 61 ).
    mo_docking->set_width( lv_width ).
  ENDMETHOD.

  METHOD get_instance.
    IF mo_instance IS NOT BOUND.
      mo_instance = NEW #( ).
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD set_criteria.
    ms_criteria = is_criteria.
    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD execute.
    IF ms_criteria-scan_packages[] IS INITIAL.
      MESSAGE s145(pak) DISPLAY LIKE rs_c_error. " Задайте пакет
      RETURN.
    ENDIF.
    cl_uml_cache=>get_singleton( )->clear_type_cache( ).
    TRY.
        DATA(lo_uml) = NEW zcl_uml_class_scanner( ).
        lo_uml->set_scanner_configuration(
            scan_local_types = ms_criteria-scan_local_types
            scan_programs = ms_criteria-scan_programs
            scan_function_groups = ms_criteria-scan_function_groups
            add_structures = ms_criteria-add_structures
            scan_deep = ms_criteria-scan_deep
            scan_used_types = ms_criteria-scan_used_types
        ).
        lo_uml->execute(
          scan_packages = ms_criteria-scan_packages
          scan_types = ms_criteria-scan_types
        ).
        mv_uml_text = lo_uml->build_plant_uml( CORRESPONDING #( ms_criteria ) ).
        display_uml( ).
      CATCH cx_root INTO DATA(lx_uml).
        MESSAGE lx_uml TYPE rs_c_info DISPLAY LIKE rs_c_error.
    ENDTRY.
  ENDMETHOD.

  METHOD display_uml.
    DATA(lv_html) = |<html>|
                 && |<body>|
                 && |<iframe id="iframe" src="{ mc_base_url }" style="width: 100%;height:100%;border:0;margin:0;padding:0;"></iframe>|
                 && |<script>|
                 && |var iframe = window.document.getElementById("iframe");|
                 && 'iframe.onload = function(){'
                 && |iframe.contentWindow.postMessage("{ get_encoded_uml( ) }", "*");|
                 && '}'
                 && |</script>|
                 && |</body>|
                 && |</html>|.
    DATA lv_xhtml TYPE xstring.
    DATA lt_html_lines TYPE swxmlcont.
    DATA lv_html_size TYPE i.
    DATA lv_html_url TYPE c LENGTH 500.

    DO 1 TIMES.
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv_html
        IMPORTING
          buffer = lv_xhtml
        EXCEPTIONS
          OTHERS = 4.
      CHECK sy-subrc EQ 0.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_xhtml
        IMPORTING
          output_length = lv_html_size
        TABLES
          binary_tab    = lt_html_lines.
      CHECK sy-subrc EQ 0.

      CALL METHOD mo_html_viewer->load_data
        EXPORTING
          type         = 'TEXT'
          subtype      = 'HTML'
          size         = lv_html_size
        IMPORTING
          assigned_url = lv_html_url
        CHANGING
          data_table   = lt_html_lines
        EXCEPTIONS
          OTHERS       = 4.
      CHECK sy-subrc EQ 0.

      CALL METHOD mo_html_viewer->show_url
        EXPORTING
          url    = lv_html_url
        EXCEPTIONS
          OTHERS = 4.
    ENDDO.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE rs_c_success NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
    ENDIF.
  ENDMETHOD.

  METHOD get_encoded_uml.
    TYPES ltv_base64 TYPE c LENGTH 65.
    CONSTANTS lc_standard TYPE ltv_base64 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='.
    CONSTANTS lc_plantuml TYPE ltv_base64 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_0'.
    DATA lv_bin TYPE xstring.
    TRY.
        DATA(lo_conv) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        CALL METHOD lo_conv->convert
          EXPORTING
            data   = mv_uml_text
          IMPORTING
            buffer = lv_bin.
        CALL METHOD cl_abap_gzip=>compress_binary
          EXPORTING
            raw_in         = lv_bin
            compress_level = 9
          IMPORTING
            gzip_out       = lv_bin.

        DATA(lv_base64) = cl_http_utility=>encode_x_base64( lv_bin ).
        rv_url = translate( val = lv_base64 from = lc_standard to = lc_plantuml ).
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root TYPE rs_c_info DISPLAY LIKE rs_c_error.
    ENDTRY.
  ENDMETHOD.

  METHOD open_browser.
    DATA(lv_url) = |{ mc_base_url }?s={ get_encoded_uml( ) }|.
    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document = lv_url
      EXCEPTIONS
        OTHERS   = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE rs_c_success NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_ss DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS set_title
      IMPORTING iv_text TYPE clike.

    CLASS-METHODS set_frame_text
      IMPORTING iv_name TYPE clike
                iv_text TYPE clike.

    CLASS-METHODS set_param_text
      IMPORTING i_param TYPE any
                iv_text TYPE clike.

    CLASS-METHODS steal_function
      IMPORTING iv_ucomm          TYPE sy-ucomm
      RETURNING VALUE(rs_functxt) TYPE smp_dyntxt.

    CLASS-METHODS create_function
      IMPORTING iv_icon_id        TYPE icon-id
      RETURNING VALUE(rs_functxt) TYPE smp_dyntxt.
ENDCLASS.

CLASS lcl_ss IMPLEMENTATION.
  METHOD set_title.
    SET TITLEBAR '%_T' OF PROGRAM 'RSSYSTDB' WITH iv_text.
  ENDMETHOD.

  METHOD set_frame_text.
    LOOP AT SCREEN.
      CHECK screen-group3 EQ 'BLK'.
      CHECK iv_name EQ screen-name+2(3).
      ASSIGN (screen-name) TO FIELD-SYMBOL(<lv_frame>).
      IF sy-subrc EQ 0.
        <lv_frame> = iv_text.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_param_text.
    DESCRIBE FIELD i_param TYPE DATA(lv_type).
    DATA(lv_range) = xsdbool( lv_type EQ 'u' ).
    IF lv_range IS INITIAL.
      ASSIGN i_param TO FIELD-SYMBOL(<l_input>).
    ELSE.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE i_param TO <l_input>.
    ENDIF.
    CHECK <l_input> IS ASSIGNED.

    LOOP AT SCREEN.
      CHECK screen-name IS NOT INITIAL.
      ASSIGN (screen-name) TO FIELD-SYMBOL(<l_param>).
      CHECK sy-subrc EQ 0.
      CHECK REF #( <l_input> ) EQ REF #( <l_param> ).
      IF screen-group3 EQ 'COM'.
        <l_input> = iv_text.
        EXIT.
      ENDIF.
      DATA(lv_text) = '%_'
        && COND #( WHEN lv_range EQ abap_true
                   THEN substring_before( val = screen-name sub = '-' )
                   ELSE screen-name )
        && '_%_APP_%-TEXT'.
      ASSIGN (lv_text) TO FIELD-SYMBOL(<lv_text>).
      IF sy-subrc EQ 0.
        <lv_text> = iv_text.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD steal_function.
    DATA lv_status TYPE sy-pfkey.
    DATA lv_program TYPE sy-repid.
    DATA lt_exclude TYPE TABLE OF sy-ucomm.
    DATA lt_function TYPE TABLE OF rsmpe_funl.
    FIELD-SYMBOLS <ls_function> TYPE rsmpe_funl.

    GET PF-STATUS lv_status PROGRAM lv_program EXCLUDING lt_exclude.
    COLLECT iv_ucomm INTO lt_exclude.

    CALL FUNCTION 'RS_CUA_GET_STATUS_FUNCTIONS'
      EXPORTING
        language      = sy-langu
        program       = lv_program
        status        = lv_status
      TABLES
        function_list = lt_function.
    ASSERT line_exists( lt_function[ fcode = iv_ucomm ] ).
    ASSIGN lt_function[ fcode = iv_ucomm ] TO <ls_function>.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = lv_status
        p_program = lv_program
      TABLES
        p_exclude = lt_exclude.

    rs_functxt = VALUE #(
        icon_id = <ls_function>-icon_id
        quickinfo = <ls_function>-text
    ).
  ENDMETHOD.

  METHOD create_function.
    SELECT SINGLE id AS icon_id
                  quickinfo AS quickinfo
      INTO CORRESPONDING FIELDS OF rs_functxt
      FROM icont
      WHERE id EQ iv_icon_id
        AND langu EQ sy-langu.
    IF sy-subrc NE 0.
      rs_functxt = VALUE #( icon_id = iv_icon_id ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

TABLES: sscrfields, tadir.
SELECTION-SCREEN FUNCTION KEY 1.                     "#EC CI_USE_WANTED
SELECTION-SCREEN FUNCTION KEY 2.                     "#EC CI_USE_WANTED

SELECTION-SCREEN BEGIN OF BLOCK scanner_settings WITH FRAME TITLE text-scn NO INTERVALS.
SELECT-OPTIONS s_scan_p FOR tadir-devclass NO INTERVALS.
SELECT-OPTIONS s_scan_o FOR tadir-obj_name NO INTERVALS.
PARAMETERS f_scan_l TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_scan_p TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_scan_f TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_scan_s TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_scan_d TYPE flag AS CHECKBOX DEFAULT ' '.
PARAMETERS f_scan_u TYPE flag AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK scanner_settings.

SELECTION-SCREEN BEGIN OF BLOCK display_settings WITH FRAME TITLE text-dis NO INTERVALS.
SELECT-OPTIONS s_disp_p FOR tadir-devclass NO INTERVALS.
SELECT-OPTIONS s_disp_o FOR tadir-obj_name NO INTERVALS.
PARAMETERS f_disp_a TYPE flag AS CHECKBOX DEFAULT ' '.
PARAMETERS f_disp_m TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_disp_e TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_disp_f TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_disp_x TYPE flag AS CHECKBOX DEFAULT ' '.
PARAMETERS f_publ TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_priv TYPE flag AS CHECKBOX DEFAULT 'X'.
PARAMETERS f_prot TYPE flag AS CHECKBOX DEFAULT ' '.
PARAMETERS f_excl_v TYPE flag AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK display_settings.

INITIALIZATION.
  s_disp_p[] = VALUE #( ( sign = 'I' option = 'CP' low = 'Z*' ) ).
  sscrfields-functxt_01 = lcl_ss=>steal_function( 'ONLI' ).
  sscrfields-functxt_02 = lcl_ss=>create_function( icon_url ).
  lcl_app=>get_instance( ).
  CASE sy-langu.
    WHEN 'R'. " Russian
      lcl_ss=>set_title( 'UML диаграмма классов' ).

      lcl_ss=>set_frame_text( iv_name = 'SCN' iv_text = 'Параметры сканера объектов' ).
      lcl_ss=>set_param_text( i_param = s_scan_p iv_text = 'Пакет' ).
      lcl_ss=>set_param_text( i_param = s_scan_o iv_text = 'Имя объекта' ).
      lcl_ss=>set_param_text( i_param = f_scan_l iv_text = 'Анализировать локальные объекты' ).
      lcl_ss=>set_param_text( i_param = f_scan_p iv_text = 'Анализировать программы' ).
      lcl_ss=>set_param_text( i_param = f_scan_f iv_text = 'Анализировать группы функций' ).
      lcl_ss=>set_param_text( i_param = f_scan_s iv_text = 'Структуры (<<data Type>>)' ).
      lcl_ss=>set_param_text( i_param = f_scan_d iv_text = 'Глубокий анализ' ).
      lcl_ss=>set_param_text( i_param = f_scan_u iv_text = 'Зависимости' ).

      lcl_ss=>set_frame_text( iv_name = 'DIS' iv_text = 'Параметры отображения объектов' ).
      lcl_ss=>set_param_text( i_param = s_disp_p iv_text = 'Пакет' ).
      lcl_ss=>set_param_text( i_param = s_disp_o iv_text = 'Имя объекта' ).
      lcl_ss=>set_param_text( i_param = f_disp_a iv_text = 'Атрибуты' ).
      lcl_ss=>set_param_text( i_param = f_disp_m iv_text = 'Методы' ).
      lcl_ss=>set_param_text( i_param = f_disp_e iv_text = 'События' ).
      lcl_ss=>set_param_text( i_param = f_disp_f iv_text = 'Дружественные классы' ).
      lcl_ss=>set_param_text( i_param = f_disp_x iv_text = 'Исключения' ).
      lcl_ss=>set_param_text( i_param = f_publ iv_text = 'Отображать Public' ).
      lcl_ss=>set_param_text( i_param = f_priv iv_text = 'Отображать Protected' ).
      lcl_ss=>set_param_text( i_param = f_prot iv_text = 'Отображать Private' ).
      lcl_ss=>set_param_text( i_param = f_excl_v iv_text = 'Исключить ракурсы ведения' ).
    WHEN OTHERS.
      lcl_ss=>set_title( 'UML class diagram' ).

      lcl_ss=>set_frame_text( iv_name = 'SCN' iv_text = 'Object scanner options' ).
      lcl_ss=>set_param_text( i_param = s_scan_p iv_text = 'Package' ).
      lcl_ss=>set_param_text( i_param = s_scan_o iv_text = 'Object Name' ).
      lcl_ss=>set_param_text( i_param = f_scan_l iv_text = 'Analyze Local Objects' ).
      lcl_ss=>set_param_text( i_param = f_scan_p iv_text = 'Analyze programs' ).
      lcl_ss=>set_param_text( i_param = f_scan_f iv_text = 'Analyze Function Groups' ).
      lcl_ss=>set_param_text( i_param = f_scan_s iv_text = 'Structures (<<data Type>>)' ).
      lcl_ss=>set_param_text( i_param = f_scan_d iv_text = 'Deep Analysis' ).
      lcl_ss=>set_param_text( i_param = f_scan_u iv_text = 'Dependency (<<uses>>)' ).

      lcl_ss=>set_frame_text( iv_name = 'DIS' iv_text = 'Object display options' ).
      lcl_ss=>set_param_text( i_param = s_disp_p iv_text = 'Package' ).
      lcl_ss=>set_param_text( i_param = s_disp_o iv_text = 'Object Name' ).
      lcl_ss=>set_param_text( i_param = f_disp_a iv_text = 'Attributes' ).
      lcl_ss=>set_param_text( i_param = f_disp_m iv_text = 'Methods' ).
      lcl_ss=>set_param_text( i_param = f_disp_e iv_text = 'Events' ).
      lcl_ss=>set_param_text( i_param = f_disp_f iv_text = 'Friends' ).
      lcl_ss=>set_param_text( i_param = f_disp_x iv_text = 'Exceptions' ).
      lcl_ss=>set_param_text( i_param = f_publ iv_text = 'Display Public' ).
      lcl_ss=>set_param_text( i_param = f_priv iv_text = 'Display Protected' ).
      lcl_ss=>set_param_text( i_param = f_prot iv_text = 'Display Private' ).
      lcl_ss=>set_param_text( i_param = f_excl_v iv_text = 'Ignore maintenance views' ).
  ENDCASE.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      lcl_app=>get_instance( )->set_criteria( VALUE #(
          scan_local_types = f_scan_l
          scan_programs = f_scan_p
          scan_function_groups = f_scan_f
          add_structures = f_scan_s
          scan_deep = f_scan_d
          scan_used_types = f_scan_u
          scan_packages = s_scan_p[]
          scan_types = s_scan_o[]
          display_packages = s_disp_p[]
          display_types = s_disp_o[]
          display_attributes = f_disp_a
          display_methods = f_disp_m
          display_events = f_disp_e
          display_friends = f_disp_f
          display_exceptions = f_disp_x
          exclude_maintenance_views = f_excl_v
          display_public = f_publ
          display_protected = f_priv
          display_private = f_prot
      ) )->execute( ).
    WHEN 'FC02'.
      lcl_app=>get_instance( )->open_browser( ).
  ENDCASE.