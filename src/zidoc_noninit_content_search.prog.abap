*&---------------------------------------------------------------------*
*& Report zidoc_noninit_content_search
*&---------------------------------------------------------------------*
*& Search in IDoc segments where the given field is not initial.
*&
*& Purpose:
*&  The aim of this report is to allow generic search in IDoc
*&  segment content, where a given field value is not initial.
*&  Since in the IDoc fields the numbers can are stored as text,
*&  the initial values of a numeric type will be stored as text as well
*&  like 0.0 etc.
*&  Transaction WE09 provides opportunity for an exact value only, and
*&  for numbers You would need the original ABAP type of the number and
*&  a concrete value to find an IDoc where the given field is filled in
*&  when looking for test data, this would require additional efforts
*&  and knowledge on the source module.
*&---------------------------------------------------------------------*
REPORT zidoc_noninit_content_search.

PARAMETERS:
  p_segtyp TYPE edilsegtyp MATCHCODE OBJECT edi_segtyp OBLIGATORY,
  p_fname  TYPE fieldnam30 OBLIGATORY.

DATA:
  segment_data    TYPE REF TO data,
  segment_matches TYPE STANDARD TABLE OF edid4.

FIELD-SYMBOLS:
  <segment_data>  TYPE any.


START-OF-SELECTION.

  TRY.
      "Fetch IDoc content Data
      SELECT * FROM edid4 INTO TABLE @DATA(segment_records)
        WHERE segnam = @p_segtyp.

      IF segment_records IS INITIAL.
        MESSAGE 'No IDocs with this segment found' TYPE 'I'.
        RETURN.
      ENDIF.

      "Create structured data
      CREATE DATA segment_data TYPE (p_segtyp).

      ASSIGN segment_data->* TO <segment_data>.
      IF sy-subrc <> 0.
        MESSAGE 'Fatal Error' TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      "Process data records converting plain text to typed ABAP structure
      LOOP AT segment_records ASSIGNING FIELD-SYMBOL(<segment_record>).
        <segment_data> = <segment_record>-sdata.
        ASSIGN COMPONENT p_fname OF STRUCTURE <segment_data> TO FIELD-SYMBOL(<field_value>).

        IF sy-subrc = 0."The field is present in the structure
          IF <field_value> IS NOT INITIAL.

            TRY.
                <field_value> = <field_value> * 1."To avoid convt_number exception with simple comparison, because it cannot be caught
                IF <field_value> = 0.
                  CONTINUE.
                ENDIF.
              CATCH cx_root.
            ENDTRY.

            APPEND <segment_record> TO segment_matches.
          ENDIF.
          UNASSIGN <field_value>.
        ENDIF.
      ENDLOOP.

    CATCH cx_root INTO DATA(ex).
      MESSAGE ex->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  IF segment_matches IS INITIAL.
    MESSAGE 'No IDocs found where this field has a value' TYPE 'I'.
  ENDIF.


  "Display IDoc segments where the given field was not initial
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name = 'EDID4'
    TABLES
      t_outtab         = segment_matches
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
