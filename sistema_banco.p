/*criar um banco*/

def var i-conta      as inte no-undo.
def var i-conta_1    as inte no-undo.
def var c-nome       as char no-undo.
def var d-valor      as decimal no-undo.
def var c-operacao   as char no-undo format "x".
def var d-limite     as decimal no-undo.
def var l-achou      as logical no-undo.

form
    "             R - Retirada"             
    "                                                         D - Depositar"            skip
    "             C - Cadastrar"            
    "                                                      E - Encerrar conta"       skip
    "             I - Reiniciar banco"      
    "                                               M - Movimentações"        skip
    "             T - Transferir"          
    "                                                      X - Extrato da sua conta" 
    "             S - Sair"                 skip
    c-operacao label "                                              Digite a Operação"
        with frame f1 row  3 center width 80
            title "Digite a operação que deseja realiza".

def frame f2 
    i-conta  label  "Conta"
    d-valor  label  "Valor"
        with frame  f2 row 6 center width 80 
            title "Digite seus dados".

def frame f4
    i-conta  label "Conta"
    c-nome   label "Nome"
    d-valor  label "Valor"
    d-limite label "Limite"
        with frame f4 row 6 center width 80
            title "Digite sua informações para criar a conta".
def frame f5
    i-conta label "Conta"
        with frame f5 row 6 center width 80
            title "Digite os dados para encerrar a conta".

def frame f7
    i-conta   label "Sua Conta"
    i-conta_1 label "Destinatario"
    d-valor   label "Valor"
        with frame f7 row 6 center width 80
            title "Digite os dados para encerrar a conta".

repeat:

    update
        c-operacao
        with frame f1 side-labels three-d.

    case c-operacao:
        when "R" then run pi-retira.
    
        when "D" then run pi-deposito.
    
        when "C" then run pi-cadastro.
    
        when "E" then run pi-encerra.
    
        when "I" then run pi-reinicio.

        when "M" then run pi-moviment.

        when "T" then run pi-transferir.

        when "X" then run pi-extrato_conta.

        when "S" then leave.
    
        otherwise message "Operação invalida" view-as alert-box.
    end case.

    assign c-operacao = "".
    
end.

procedure pi-retira:

    def var l-achou as log no-undo init no.

    update
        i-conta
        d-valor
            with frame f2 side-labels three-d.

    if(d-valor <= 0)                                 
    then do:
        message "Saldo invalido"
            view-as alert-box.
        return.
    end.
    

   find first banco 
        where banco.conta = i-conta
              exclusive-lock.

        if ( banco.saldo < d-valor )
        then do:
             MESSAGE "saldo invalido"
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
             return.
        end.

        assign banco.saldo = banco.saldo - d-valor
               l-achou = true.

        run pi-registra-movimento ( input "Retirada",
                                    input "D",
                                    input banco.saldo,
                                    input banco.conta ).

        disp                                      
            banco.conta
            banco.nome
            banco.saldo
            banco.limite
            with frame f3 center row 10.



    

    if ( not l-achou )
    then
        MESSAGE "nao achou"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

/*
    find first banco
         where banco.conta = i-conta
               exclusive-lock.

    if not avail banco
    then do:
        message "Conta Invalida"
          view-as alert-box.
        return.
    end.

    if (banco.saldo < d-valor) 
    then do:
        message "Valor maior que seu saldo"
            view-as alert-box.
        return.
    end.      


    assign banco.saldo = banco.saldo - d-valor.

    disp                                      
        banco.conta
        banco.nome
        banco.saldo
        banco.limite
            with frame f3 center row 10. */

    release banco.

    assign i-conta = 0  
           d-valor = 0.

end procedure.


procedure pi-deposito:

    update
        i-conta
        d-valor
            with frame f2 side-labels three-d.

    
    
                       
    if(d-valor < 0)
    then do:
         message "Deposito invalido"
            view-as alert-box.
         return.
    end.
    
    find first banco 
         where banco.conta = i-conta
               exclusive-lock.
    
    if not avail banco
    then do:
        message "Conta Invalida"
            view-as alert-box.
        return.
    end.


    assign banco.saldo = banco.saldo + d-valor.

    run pi-registra-movimento ( input "Depósito",
                                input "C",
                                input banco.saldo,
                                input banco.conta ).  
    
    disp banco.conta
         banco.nome
         banco.saldo
         banco.limite
         with frame f3 center row 10.

    assign i-conta = 0  
           d-valor = 0.

    release banco.

end procedure.



procedure pi-cadastro:

    update
        i-conta 
        c-nome  
        d-valor 
        d-limite
            with frame f4 side-labels three-d.

    if(d-valor < 0)
    then do:
        message "Valor incorreto"
            view-as alert-box.
        return.
    end.

    if(d-limite < 0)
    then do:
        message "Limite incorreto"
            view-as alert-box.
        return.
    end.

    find first banco 
         where banco.conta = i-conta 
               no-lock no-error.
    
    if avail banco
    then do:
        message "Conta já existente"
            view-as alert-box.
        return.
    end.
  
    create banco.
    assign banco.conta  = i-conta
           banco.nome   = c-nome
           banco.saldo  = d-valor
           banco.limite = d-limite.

    run pi-registra-movimento ( input "Abertura cont",
                                input "C",
                                input banco.saldo,
                                input banco.conta ).

    disp
        banco.conta
        banco.nome
        banco.saldo
        banco.limite
            with frame f3 center row 10.

    assign i-conta  = 0  
           c-nome   = "" 
           d-valor  = 0  
           d-limite = 0. 

    release banco.
    
end procedure.

procedure pi-encerra:

    update
        i-conta 
           with frame f5 side-labels three-d.

    find first banco 
         where banco.conta = i-conta
            exclusive-lock.

    if avail banco
    then do:

         message banco.conta  
                 banco.nome   
                 banco.saldo  
                 banco.limite 
                 "Deseja encerrar essa conta"
                   view-as alert-box question
                       button yes-no update l-encerra as log.

         run pi-registra-movimento ( input "Ecerramento cont", 
                                     input "D",                
                                     input banco.saldo,        
                                     input banco.conta ).

         if ( l-encerra )
         then do: 
              delete banco.
              message "Conta encerrada com sucesso"
                 view-as alert-box.
         end.
            
    end.

    assign i-conta = 0.

end procedure.

procedure pi-reinicio:

    message "Deseja reiniciar as informações do banco ?"
        view-as alert-box question
            button yes-no update l-reinicio as log.

    if not l-reinicio then return.

     for each banco exclusive-lock:
         delete banco.
     end.

     for each moviment exclusive-lock:
         delete moviment.
     end.

     create banco.
     assign banco.conta  = 1
            banco.nome   = "Giovana"
            banco.saldo  = 500
            banco.limite = 100.

     create banco.
     assign banco.conta  = 2
            banco.nome   = "Patrick"
            banco.saldo  = 1000
            banco.limite = 100.

     create banco.
     assign banco.conta  = 3
            banco.nome   = "Daniel"
            banco.saldo  = 300
            banco.limite = 50.

     create banco.
     assign banco.conta  = 4
            banco.nome   = "Toshyro"
            banco.saldo  = -20
            banco.limite = 50.

     release banco.

     message "Banco de dados reiciado"
         view-as alert-box.

end procedure.

procedure pi-moviment:

    for each moviment no-lock:
        disp moviment.
    end.

end procedure.

procedure pi-registra-movimento:

    def input param c-descr like moviment.descricao no-undo.
    def input param c-tipo  as char no-undo.
    def input param d-valor like moviment.valor no-undo.
    def input param i-conta like moviment.conta no-undo.
    
    create moviment.
    assign moviment.seq         = next-value(s-extrato)
           moviment.descricao   = c-descr
           moviment.tipo        = c-tipo
           moviment.valor       = d-valor
           moviment.conta       = i-conta.

    release moviment.



end procedure.

procedure pi-transferir:

    update
        i-conta
        i-conta_1
        d-valor
        with frame f7 side-labels three-d.

    if not can-find ( banco where banco.conta = i-conta_1) or not can-find ( banco where banco.conta = i-conta)
    then do:
         message "Conta invalida"
             view-as alert-box.
    end.

    for first banco 
        where banco.conta = i-conta
              exclusive-lock.
    end.

    if ( banco.saldo < d-valor )
    then do:
         MESSAGE "saldo invalido"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
         return.
    end.
    
    assign banco.saldo = banco.saldo - d-valor.

    run pi-registra-movimento ( input "Transferencia",
                                input "D",
                                input d-valor,
                                input banco.conta ).

    for first banco
        where banco.conta = i-conta_1
            exclusive-lock.
    end.

    assign banco.saldo = banco.saldo + d-valor.

    message "Transferencia feita para" banco.conta banco.nome
        view-as alert-box.


    run pi-registra-movimento ( input "Transferencia",
                                input "C",
                                input d-valor,
                                input banco.conta ).

    assign i-conta   = 0 
           i-conta_1 = 0 
           d-valor   = 0.

end procedure.


procedure pi-extrato_conta:

    update 
        i-conta
            with frame f5 side-labels three-d.

    for each moviment
        where moviment.conta = i-conta
            no-lock:
    disp
        moviment.seq
        moviment.descricao
        moviment.tipo
        moviment.valor
        moviment.conta
           with frame f3 down center row 10.
    end.

    assign i-conta = 0.

end procedure.













