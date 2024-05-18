let unused () = assert false

let oo_prim = Lambda.transl_prim "CamlinternalOO"

let share _cst = unused ()

let meth_tag s = Lambda.Lconst(Const_base(Const_int(Btype.hash_variant s)))

let meth _obj lab =
  let tag = meth_tag lab in
  (tag, [])

let reset_labels () = ()

let transl_label_init f = f ()

let transl_store_label_init _id size f x = size, f x

let method_ids = ref Ident.Set.empty

let oo_wrap _env _cache_required f x = f x

let oo_wrap_gen _env _cache_required f x = f x

let oo_add_class _id = unused ()

let reset () = ()
