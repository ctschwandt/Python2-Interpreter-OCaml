type expr = Int_Expr of int
          | Float_Expr of float
          | Bool_Expr of bool
          | String_Expr of string
          | Var_Expr of string
          | List_Expr of expr list
          | Tuple_Expr of expr list
          | Dict_Expr of (expr * expr) list
          | Index_Expr of expr * expr
          | Neg_Expr of expr
          | Add_Expr of expr * expr
          | Sub_Expr of expr * expr
          | Mul_Expr of expr * expr
          | Div_Expr of expr * expr
          | Pow_Expr of expr * expr
          | Bitand_Expr of expr * expr
          | Bitor_Expr of expr * expr
          | Bitxor_Expr of expr * expr
          | Lshift_Expr of expr * expr
          | Rshift_Expr of expr * expr
          | Is_Eq_Expr of expr * expr
          | Is_Neq_Expr of expr * expr
          | Lt_Expr of expr * expr
          | Leq_Expr of expr * expr
          | Gt_Expr of expr * expr
          | Geq_Expr of expr * expr
          | And_Expr of expr * expr
          | Or_Expr of expr * expr
          | Not_Expr of expr
          | Bitnot_Expr of expr
and
  target = Name_Target of string
         | Index_Target of target * expr
and
  else_part = No_Else
            | Else_Block of stmt list
and
  stmt = Expr_Stmt of expr
       | Print_Stmt of expr
       | Assign_Stmt of string list * expr
       | Target_Assign_Stmt of target * expr
       | Exit_Stmt
       | If_Stmt of expr * stmt list * else_part
       | While_Stmt of expr * stmt list
;;
