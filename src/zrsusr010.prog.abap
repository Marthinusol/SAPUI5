************************************************************************
*  17.03.2006 R000731 ( Note 932726 )                            4.6b+ *
*  follow up correction to note 826908 (AGR_1016-PSTATE no longer used)*
*----------------------------------------------------------------------*
* 06.04.2006 R00731 (note 915638)                                4.6b+ *
* optimization of behavior in selection screen variant handling        *
*
*  20.06.2009 D034973  ( Note 1352248 )                            4.6C+
* transaction's text will be get with the new
* the new f.m. SUSR_READ_TCODE_STD_TEXTS
*----------------------------------------------------------------------*

include sr010top.

selection-screen skip 2.
* Selektionskriterien erfragen
* Standardselektionen wie Benutzer, Profil, Objekt und Berechtigung
selection-screen begin of block standard
                 with frame title text-008
                 no intervals.
selection-screen begin of line.
parameters     us radiobutton group tcd.
selection-screen comment (20) text-001 for field us .
selection-screen end   of line.
parameters     user like ust04-bname memory id xus
                    matchcode object user_comp.
*SELECTION-SCREEN SKIP 1.
selection-screen begin of line. " note 636507
parameters role radiobutton group tcd.
selection-screen comment (20) text-006 for field role.
selection-screen end of line.
parameters roles like agr_define-agr_name.
*SELECTION-SCREEN SKIP 1.
selection-screen begin of line.
parameters     pr radiobutton group tcd.
selection-screen comment  (20) text-002 for field pr.
selection-screen end   of line.
parameters     prof  like usr10-profn memory id xup.
*SELECTION-SCREEN SKIP 1.
selection-screen begin of line.
parameters     au radiobutton group tcd.
selection-screen comment  (20) text-004 for field au.
selection-screen end   of line.
parameters     auth  like ust12-auth memory id xua.
selection-screen end of block standard.

constants objct_name(10) type c value 'S_TCODE'.
data: objct like tobj-objct.
data: begin of non_authorized_roles occurs 0,  " note 636507
        agr_name like agr_define-agr_name,
      end of non_authorized_roles.
data roles_list like agr_define occurs 0
   with header line.
data single_agrs_in_col like agr_agrs occurs 0
   with header line.
data: wa_ust04 like ust04,
      reference_user like usrefus,
      wa_ust12 like ust12.
data:   begin of auts_r occurs 100.
        include structure usref.
data:   end of auts_r.
data: wa_usr02 like usr02,
      gv_ucomm like sy-ucomm .

*&---------------------------------------------------------------------*
*&      EVENT  AT SELECTION-SCREEN ON EXIT-COMMAND .                   *
*&---------------------------------------------------------------------*
* 06.04.2006 R00731 (note 915638)                                4.6b+ *
* optimization of behavior in selection screen variant handling        *
*----------------------------------------------------------------------*
at selection-screen on exit-command .
  gv_ucomm = sy-ucomm .
  if sy-ucomm eq 'GET' .          "avoid multiple selected radio buttons
    clear: us, role, pr, au .
  endif.

*&---------------------------------------------------------------------*
*&      EVENT  AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
* 06.04.2006 R00731 (note 915638)                                4.6b+ *
* optimization of behavior in selection screen variant handling        *
*----------------------------------------------------------------------*
at selection-screen.
  if gv_ucomm eq 'GET'.
    if sy-slset is initial
      or  ( sy-msgid = 'DB'
            and (    sy-msgno eq '273'
                  or sy-msgno eq '610'
                  or sy-msgno eq '612' )
          ) .
      clear gv_ucomm.
      exit .                        "avoid useless input value check
    endif.
  endif.
  clear gv_ucomm.

  sel = 'B'.
* Berechtigungsprüfungen
  if not us   is initial.
    if user is initial.
* Please enter a selection value
      message e852.
    else.
      clear wa_usr02.
      select single bname ustyp class from usr02 into   "note 670488
      corresponding fields of wa_usr02
             where bname = user.
      if sy-subrc <> 0.
        message e124 with  user. " Benutzer ex. nicht
      endif.
* If user has ho profiles, no futher execution
      select single profile from ust04 into wa_ust04
        where bname = user.
      if sy-subrc ne 0.
        if wa_usr02-ustyp ne 'L'.
* Check if user has a reference one with profiles
          select single * from  usrefus into reference_user
                where  bname  = user.
          if sy-subrc eq 0.
* Check if a reference user has profile(s)
            select single * from ust04
                 where bname = reference_user-refuser.
            if sy-subrc ne 0.
* Dem Benutzer & sind keine Profile zugeordnet.
              message e854 with user.
            endif.
          else.
* Dem Benutzer & sind keine Profile zugeordnet.
            message e854 with user.
          endif.
        else.
* Dem Benutzer & sind keine Profile zugeordnet.
          message e854 with user.
        endif.
      endif.
      authority-check object 'S_USER_GRP'
            id 'CLASS' field wa_usr02-class          "note 670488
            id 'ACTVT' field '03'.
      if sy-subrc <> 0.
        message e512 with wa_usr02-class.            "note 670488
      endif.
    endif.
  endif.
  if not role is initial.   " note 636507
    if roles is initial.
* Please enter a selection value
      message e852.
    else.
      refresh: single_agrs_in_col, roles_list,
        non_authorized_roles.
      perform resolve_collective_role tables single_agrs_in_col
            roles_list using roles.
      perform authority_check_and_pop_up tables roles_list
            non_authorized_roles.
    endif.
  endif.
  if not pr   is initial.
    if prof is initial.
* Please enter a selection value
      message e852.
    else.
      select single profn from ust10s into prof
             where profn = prof
             and aktps = 'A'.
      if sy-subrc <> 0.
        select single profn from ust10c into prof
               where profn = prof
                             and aktps = 'A'.
        if sy-subrc <> 0.
          message e127 with prof.   " Profil existiert nicht.
        endif.
      endif.
      authority-check object 'S_USER_PRO'
             id 'PROFILE' field prof
             id 'ACTVT' field '03'.
      if sy-subrc <> 0.
        message e511 with prof.
      endif.
    endif.
  endif.
  if not au   is initial.
    if auth is initial.
* Please enter a selection value
      message e852.
    else.
      select single auth from usr12 into auth
             where objct = objct_name
               and auth  = auth
               and aktps = 'A'.                    "note 670488
      if sy-subrc <> 0.
*      MESSAGE E129 WITH AUTH.   " Objekt existiert nicht.
* Object S_TCODE has no authorization &
        message e890 with auth.
      endif.
      authority-check object 'S_USER_AUT'
           id 'OBJECT' dummy
           id 'AUTH' field auth
           id 'ACTVT' field '03'.
      if sy-subrc <> 0.
*      MESSAGE e510 WITH auth .
* No rights to display authorization & for object S_TCODE
        message e891 with auth.
      endif.
    endif.
  endif.

start-of-selection.

  if us = 'X'.
    call function 'SUSR_TCODES_LIST_SELOPT_USER'
      exporting
        user    = user
        seltype = sel
      exceptions
        others  = 1.
  elseif role = 'X'.  " note 636507
    call function 'SUSR_TCODES_LIST_SELOPT_ROLES'
      exporting
        role       = roles
        seltype    = sel
      tables
        roles_list = roles_list.
  elseif pr = 'X'.
    call function 'SUSR_TCODES_LIST_SELOPT_PROF'
      exporting
        profile = prof
        seltype = sel
      exceptions
        others  = 1.
  elseif au = 'X'.
    clear: auts_r, auts_r[].
    select objct auth from ust12 into                   "#EC CI_GENBUFF
      corresponding fields of wa_ust12
      where auth = auth.
      auts_r-object = wa_ust12-objct.
      auts_r-auth = wa_ust12-auth.
      append auts_r.
    endselect.
    if sy-subrc eq 0.
      perform present_result using auts_r.
    endif.
  endif.

*&---------------------------------------------------------------------*
*&      Form  RESOLVE_COLLECTIVE_ROLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROLES  text
*----------------------------------------------------------------------*
form resolve_collective_role
                 tables p_single_agrs_in_col structure agr_agrs
                        p_roles_list structure roles_list
                 using  p_roles type agr_define-agr_name.
  data: it_agr_1016 like agr_1016 occurs 0 with header line.
  select single agr_name from agr_define into roles
         where agr_name = p_roles.
  if sy-subrc ne 0.
* Role does not exist
    message e853 with p_roles.
  endif.
* Check if the role is a composite one.
  refresh p_single_agrs_in_col.
  call function 'PGRN_CHECK_COLLECTIVE_AGR'
    exporting
      activity_group                = p_roles
    exceptions
      activity_group_does_not_exist = 1
      activity_group_is_collective  = 2
      others                        = 3.
  if sy-subrc = 1.
* Role does not exist
    message e853 with p_roles.
  elseif sy-subrc = 0.
* Role is not collective one
    refresh p_roles_list.
    select * from agr_1016 into it_agr_1016
         where agr_name = p_roles
           and generated = 'X' .                         "note 932726
    endselect.
    if sy-subrc eq 0.
      p_roles_list-agr_name = p_roles.
      append p_roles_list.
    else.
* Es existiert kein Profil zur Rolle &.
      message e857 with p_roles.
    endif.
  elseif sy-subrc = 2.
* Role is a collective one
    select * from agr_agrs into table p_single_agrs_in_col
         where agr_name = p_roles
         and attributes = space.
    if sy-subrc ne 0.
* Collective role has no single ones
      refresh p_roles_list.
* Die Sammelrolle & enthält keine Einzelrollen.
      message e855 with p_roles.
    else.
* Collective role has some single ones
      refresh p_roles_list.
      loop at p_single_agrs_in_col.
        if sy-tabix eq 1.
          p_roles_list-agr_name = p_roles.
          append p_roles_list.
          p_roles_list-agr_name = p_single_agrs_in_col-child_agr.
          append p_roles_list.
        else.
          p_roles_list-agr_name = p_single_agrs_in_col-child_agr.
          append p_roles_list.
        endif.
      endloop.
    endif.
* Check if there is at least a role with generated profile
    if not p_roles_list[] is initial.
      select * from agr_1016 into it_agr_1016
           for all entries in roles_list
           where agr_name = roles_list-agr_name
             and generated = 'X' .                         "note 932726
      endselect.
      if sy-subrc ne 0.
* Keine Profile zu den enthaltenen Einzelrollen.
        message e856.
      endif.
    endif.
  endif.
endform.                    " RESOLVE_COLLECTIVE_ROLE

*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK_AND_POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROLES_LIST  text
*----------------------------------------------------------------------*
form authority_check_and_pop_up
     tables   p_roles_list structure roles_list
              p_non_authorized_roles structure non_authorized_roles.
  data:  number_non_authorized_roles type i,
         okcode type c.

  data: list_column_headers type susr_popup_list_cln_headers.

  loop at p_roles_list.
    authority-check object 'S_USER_AGR'
             id 'ACT_GROUP' field p_roles_list-agr_name
             id 'ACTVT' field '03'.
    if sy-subrc ne 0.
      p_non_authorized_roles-agr_name = p_roles_list-agr_name.
      append p_non_authorized_roles.
    endif.
  endloop.
  describe table p_non_authorized_roles
    lines number_non_authorized_roles.
  if not p_non_authorized_roles is initial.
    if number_non_authorized_roles eq 1.
* No authorization to display role &
      message e422(s#) with p_non_authorized_roles-agr_name.
    else.
      list_column_headers-cln1_lng_txt = 'Role'(201).
      call function 'SUSR_POPUP_LIST_WITH_TEXT'
        exporting
          text1               = 'List of roles you are not authorized to display:'(r01)
          text2               = 'Action will be cancelled.'(r02)
          list_column_headers = list_column_headers
        importing
          ok_code             = okcode
        tables
          list                = p_non_authorized_roles.
      refresh: p_non_authorized_roles, p_roles_list.
*     Die Aktion wurde abgebrochen
      message e232(s#).
    endif.
  endif.
endform.                    " AUTHORITY_CHECK_AND_POP_UP
*&---------------------------------------------------------------------*
*&      Form  PRESENT_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AUTS_R  text
*----------------------------------------------------------------------*
form present_result  using    p_auts_r.
  data:   begin of tcodes occurs 100.
          include structure ussel.
  data:   end of tcodes.

  data:   begin of tcodes1 occurs 100.
          include structure ussel1.
  data:   end of tcodes1.
  refresh tcodes.
  refresh tcodes1.
  call function 'SUSR_GET_TCODES_WITH_AUTH'
    tables
      auths  = auts_r
      tcodes = tcodes
    exceptions
      others = 1.

  tcodes1-seltype = 'T'.
  loop at tcodes.
    move-corresponding tcodes to tcodes1.
    append tcodes1.
  endloop.

  refresh tcodes.
  call function 'SUSR_GET_TCODES_AUTH_S_TCODE'
    tables
      auths  = auts_r
      tcodes = tcodes
    exceptions
      others = 1.

  tcodes1-seltype = 'S'.
  loop at tcodes.
    move-corresponding tcodes to tcodes1.
    append tcodes1.
  endloop.


  call function 'SUSR_TCODES_LIST_BOTH'
    exporting
      seltype = 'AU'
      name    = space
      name2   = auth
      compare = sel
    tables
      tcodes  = tcodes1
    exceptions
      others  = 1.

endform.                    " PRESENT_RESULT
