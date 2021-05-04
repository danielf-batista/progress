&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          testedb          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-Customer NO-UNDO LIKE Customer.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var c-new-upd  as char  no-undo init "".
def var r-outvalor as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-Customer

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME tt-Customer.Cust-Num 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME tt-Customer.Cust-Num 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME tt-Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME tt-Customer
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH tt-Customer SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH tt-Customer SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME tt-Customer
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME tt-Customer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-Customer.Cust-Num Customer.Name ~
Customer.City 
&Scoped-define ENABLED-TABLES tt-Customer Customer
&Scoped-define FIRST-ENABLED-TABLE tt-Customer
&Scoped-define SECOND-ENABLED-TABLE Customer
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 IMAGE-1 bt-01 bt-02 bt-04 ~
bt-06 bt-brwclientes bt-vapara bt-novo bt-edit bt-delete bt-copi 
&Scoped-Define DISPLAYED-FIELDS tt-Customer.Cust-Num Customer.Name ~
Customer.City 
&Scoped-define DISPLAYED-TABLES tt-Customer Customer
&Scoped-define FIRST-DISPLAYED-TABLE tt-Customer
&Scoped-define SECOND-DISPLAYED-TABLE Customer


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-01 
     LABEL "<<" 
     SIZE 8 BY 1.57 TOOLTIP "Ultimo".

DEFINE BUTTON bt-02 
     LABEL "<" 
     SIZE 8 BY 1.57 TOOLTIP "Anterior".

DEFINE BUTTON bt-04 
     LABEL ">" 
     SIZE 8 BY 1.57 TOOLTIP "Próximo".

DEFINE BUTTON bt-06 
     LABEL ">>" 
     SIZE 8 BY 1.57 TOOLTIP "Ultimo".

DEFINE BUTTON bt-brwclientes 
     IMAGE-UP FILE "adeicon/rbuild%.ico":U
     LABEL "Browser" 
     SIZE 8 BY 1.83 TOOLTIP "Browser".

DEFINE BUTTON bt-canc 
     LABEL "CANCELAR" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-copi 
     IMAGE-UP FILE "adeicon/firststeps.ico":U
     LABEL "Copiar" 
     SIZE 8 BY 1.83 TOOLTIP "Copiar".

DEFINE BUTTON bt-delete 
     IMAGE-UP FILE "adeicon/uninstall_oebps.ico":U
     LABEL "Excluir" 
     SIZE 8 BY 1.83 TOOLTIP "Excluir".

DEFINE BUTTON bt-edit 
     IMAGE-UP FILE "adeicon/results%.ico":U
     LABEL "Editar" 
     SIZE 8 BY 1.83 TOOLTIP "Editar".

DEFINE BUTTON bt-novo 
     IMAGE-UP FILE "adeicon/addcomponents.ico":U
     LABEL "Novo" 
     SIZE 8 BY 1.83 TOOLTIP "Novo".

DEFINE BUTTON bt-salve 
     LABEL "SALVAR" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-vapara 
     IMAGE-UP FILE "adeicon/workshp%.ico":U
     LABEL "" 
     SIZE 8 BY 1.83 TOOLTIP "Vá Para".

DEFINE IMAGE IMAGE-1
     FILENAME "C:/estudos/logo.png":U
     SIZE 37 BY 1.57.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 117 BY 2.35.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 12.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      tt-Customer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bt-01 AT ROW 2.04 COL 2 WIDGET-ID 2
     bt-02 AT ROW 2.04 COL 11 WIDGET-ID 10
     bt-04 AT ROW 2.04 COL 21 WIDGET-ID 8
     bt-06 AT ROW 2.04 COL 30 WIDGET-ID 12
     bt-brwclientes AT ROW 2.04 COL 48 WIDGET-ID 56
     bt-vapara AT ROW 2.04 COL 59 WIDGET-ID 54
     bt-novo AT ROW 2.04 COL 78 WIDGET-ID 18
     bt-edit AT ROW 2.04 COL 88 WIDGET-ID 16
     bt-delete AT ROW 2.04 COL 98 WIDGET-ID 14
     bt-copi AT ROW 2.04 COL 108 WIDGET-ID 6
     tt-Customer.Cust-Num AT ROW 6.22 COL 19 COLON-ALIGNED WIDGET-ID 60
          LABEL "Código"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1 TOOLTIP "Código"
     Customer.Name AT ROW 8.3 COL 19 COLON-ALIGNED WIDGET-ID 40
          LABEL "Nome"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     Customer.City AT ROW 10.39 COL 19 COLON-ALIGNED WIDGET-ID 32
          LABEL "Cidade"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     bt-salve AT ROW 12.48 COL 25 WIDGET-ID 42
     bt-canc AT ROW 12.48 COL 48 WIDGET-ID 46
     RECT-1 AT ROW 1.78 COL 1 WIDGET-ID 28
     RECT-2 AT ROW 4.65 COL 4 WIDGET-ID 30
     IMAGE-1 AT ROW 5.17 COL 79 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.2 BY 16.22 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-Customer T "?" NO-UNDO testedb Customer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Cadastro Clientes"
         HEIGHT             = 16.22
         WIDTH              = 120.2
         MAX-HEIGHT         = 34.83
         MAX-WIDTH          = 307.2
         VIRTUAL-HEIGHT     = 34.83
         VIRTUAL-WIDTH      = 307.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = 14
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-canc IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salve IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Customer.City IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
ASSIGN 
       Customer.City:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN tt-Customer.Cust-Num IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Customer.Name IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
ASSIGN 
       Customer.Name:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "Temp-Tables.tt-Customer"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cadastro Clientes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cadastro Clientes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-01 C-Win
ON CHOOSE OF bt-01 IN FRAME DEFAULT-FRAME /* << */
DO:
   find first customer no-lock no-error.
  if avail customer      
  then disp customer.cust-num
            customer.name
            customer.city
       with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-02 C-Win
ON CHOOSE OF bt-02 IN FRAME DEFAULT-FRAME /* < */
DO:
  find prev customer no-lock no-error.
  if avail customer      
  then disp customer.cust-num
            customer.name
            customer.city
       with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-04
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-04 C-Win
ON CHOOSE OF bt-04 IN FRAME DEFAULT-FRAME /* > */
DO:
  find next customer no-lock no-error.
  if avail customer      
  then disp customer.cust-num
            customer.name
            customer.city
      with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-06
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-06 C-Win
ON CHOOSE OF bt-06 IN FRAME DEFAULT-FRAME /* >> */
DO:
  
  find last customer no-lock no-error.
  if avail  customer      
  then disp customer.cust-num
            customer.name
            customer.city
      with frame {&frame-name}.

/*   message "Você já está no ultimo registro" */
/*             view-as alert-box.              */

 
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-brwclientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-brwclientes C-Win
ON CHOOSE OF bt-brwclientes IN FRAME DEFAULT-FRAME /* Browser */
DO:
  run C:\estudos\browsercadclientes.w(output r-outvalor).

  find first customer
       where rowid(customer) = r-outvalor
             no-lock no-error.

   if avail customer
   then disp customer.cust-num
             customer.name
             customer.city
       with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-canc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-canc C-Win
ON CHOOSE OF bt-canc IN FRAME DEFAULT-FRAME /* CANCELAR */
do:

 message "Deseja cancelar a operação?"
          view-as alert-box question
          button yes-no update l-resposta as log.

 if l-resposta
 then do:
     find prev  customer no-lock no-error.
     if avail   customer      
     then disp  customer.cust-num
                customer.name
                customer.city
                with frame {&frame-name}.
 end.

 disable bt-canc 
         bt-salve
     with frame {&frame-name}.

  enable bt-edit
         bt-delete
         bt-copi
         bt-novo
         bt-01
         bt-02
         bt-04
         bt-06
 with frame {&frame-name}.

  assign customer.name:read-only = true.
  assign customer.city:read-only = true.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-copi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-copi C-Win
ON CHOOSE OF bt-copi IN FRAME DEFAULT-FRAME /* Copiar */
DO:
  run pi-copy.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-delete C-Win
ON CHOOSE OF bt-delete IN FRAME DEFAULT-FRAME /* Excluir */
DO:
  run pi-delete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-edit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-edit C-Win
ON CHOOSE OF bt-edit IN FRAME DEFAULT-FRAME /* Editar */
DO:
    assign c-new-upd = "UPD".
    run pi-boton.
    run pi-habilita-campos (yes).

    apply "entry" to  customer.name.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-novo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-novo C-Win
ON CHOOSE OF bt-novo IN FRAME DEFAULT-FRAME /* Novo */
DO:
    assign c-new-upd = "NEW".
    run pi-boton.
    run pi-habilita-campos  (yes) .

    assign customer.cust-num:screen-value = ""
           customer.name:screen-value = ""
           customer.city:screen-value = "".

    apply "entry" to  customer.name.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salve
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salve C-Win
ON CHOOSE OF bt-salve IN FRAME DEFAULT-FRAME /* SALVAR */
do:

    case c-new-upd:
        when "NEW" then run pi-salva-new. 
        when "UPD" then run pi-salva-edit. 
    end case.

    assign c-new-upd = "".

    enable bt-edit
           bt-delete
           bt-copi
           bt-novo
           bt-01
           bt-02
           bt-04
           bt-06
           with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara C-Win
ON CHOOSE OF bt-vapara IN FRAME DEFAULT-FRAME
DO:
  run c:\estudos\irpara.w ( output r-outvalor ).

    find first customer 
         where rowid(customer) = r-outvalor 
               no-lock no-error.

    if avail customer 
    then disp customer.cust-num
              customer.name
              customer.city
              with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
 
find first tt-customer no-lock.
assign tt-customer.cust-num:screen-value = string (tt-customer.cust-num).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE Customer THEN 
    DISPLAY Customer.Name Customer.City 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE tt-Customer THEN 
    DISPLAY tt-Customer.Cust-Num 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 IMAGE-1 bt-01 bt-02 bt-04 bt-06 bt-brwclientes bt-vapara 
         bt-novo bt-edit bt-delete bt-copi tt-Customer.Cust-Num Customer.Name 
         Customer.City 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-boton C-Win 
PROCEDURE pi-boton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        
 enable bt-salve with frame {&frame-name}.
 enable bt-canc with frame {&frame-name}.
 disable bt-edit
         bt-delete
         bt-copi
         bt-01
         bt-02
         bt-04
         bt-06
 with frame {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-copy C-Win 
PROCEDURE pi-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
message "Deseja copia esse registro ?"
    view-as alert-box question
    button yes-no update l-resposta as log.

if not l-resposta then return.

find current customer exclusive-lock.

create customer.

      display customer.cust-num
      with frame {&frame-name}.

      assign customer.name = customer.name:screen-value
             customer.city = customer.city:screen-value.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-delete C-Win 
PROCEDURE pi-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
message "Deseja excluir esse registro ?"
    view-as alert-box question
    button yes-no update l-resposta as log.

if not l-resposta then return.

find current customer exclusive-lock.

if avail customer then delete customer.

message " Registro excluído com sucesso" view-as alert-box.

find prev customer no-lock no-error.

if avail customer
then disp customer.cust-num
          customer.name
          customer.city
          with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-campos C-Win 
PROCEDURE pi-habilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param l-status as log no-undo.

    assign customer.name:read-only in frame {&frame-name} = not l-status.
    assign customer.city:read-only in frame {&frame-name} = not l-status.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salva-edit C-Win 
PROCEDURE pi-salva-edit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame {&frame-name}:

  find current customer exclusive-lock.

  assign customer.name = customer.name:screen-value
         customer.city = customer.city:screen-value.

  message "Registro Salvo"
      view-as alert-box.

  enable
      bt-edit    
      bt-delete  
      bt-copi    
      bt-novo    
      bt-01
      bt-02
      bt-04
      bt-06
      with frame {&frame-name}.

  disable bt-salve with frame {&frame-name}.
  disable bt-canc with frame {&frame-name}.

  assign customer.name:read-only = true.
  assign customer.city:read-only = true.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salva-new C-Win 
PROCEDURE pi-salva-new :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

      find current customer exclusive-lock.

      create customer.

      display customer.cust-num
         with frame {&frame-name}.

      assign customer.name = customer.name:screen-value
             customer.city = customer.city:screen-value. 

      
    
      message "Cadastro conluido"
          view-as alert-box.
    
      enable
          bt-01
          bt-02
          bt-04
          bt-06
          with frame {&frame-name}.
    
      disable bt-salve with frame {&frame-name}. 
      disable bt-canc with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

