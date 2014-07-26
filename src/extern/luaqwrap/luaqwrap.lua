--!       @file  luaqwrap.lua
--      @brief  An API wrapper for the Lua programming Language
--
--     @author  Luis E. Garcia Ontanon <luis@ontanon.org>
--
--     Created  01/07/2014
--    Revision  $Id:  $
--    Compiler  gcc/g++
--   Copyright  Copyright (c) 2014, Luis E. Garcia Ontanon <luis@ontanon.org>
--
--   See LICENSE file for for Copyright notice
do
    local lqw_include="luaqwrap.h"
    local lqw_code="luaqwrap.c"
    
    local debug_filebreak = nil
    local debug_linebreak = nil
    
    local verbose = 6 -- I'm the verbosity level
    local own_prefix  = arg[0]:match("^(.*/)[%w_.]") or '' -- I hold the own prefix to the script (and its files)
    local module_name
    
    
  
    -- shortcuts
    local E = error -- the error function (will likely change at each line read)
    local f = string.format
    string.f = string.format
    local M = string.match
    string.M = M
    
    function EF(...) return E(f(...)) end
    function Fmt(fn) return function(...) return fn(f(...)) end end 

    

    function table.top(t) return t[#t] end
    function table.shift(t) return t:remove(1) end
    table.pop = table.remove
    table.push = table.insert
    
    local lqw_cmd = {}; --  //LW: commands execution
    local mode = 'static'
    
    local _R = {} -- I'm a bag of relevant objects that will be dumped on an eventual error message
    local function R(o) _R[o] = o return o end -- this is relevant
    local function IR(o) _R[o] = nil return o end -- this is not relevant
    local function NRA() _R = {} end -- nothing's relevant anymore!
    local DR -- DR() dump what's relevant (defined bellow)
    
    local function dup_to(t,n,s) -- I create tables that will copy to the given table (err if key exists)
        -- used to guarantee uniqueness of names between different tables, or if called as dup_to({}) a single table
        n = n or E("dup_to without name")
        return setmetatable(s or {},{
            __newindex=function(o,k,v)
                local e = rawget(t,k);
                if e then
                    EF("duplicate in %s: '%s' previously used in %s:%d",n,k, e.filename, e.ln);
                end
                rawset(o,k,v);
                t[k]=v;
                R(v)
                if type(v) == 'table' then
                        v.__t = n
                        if (v.__k) then
                            table.insert(v.__k,k)
                        else
                            v.__k = {k}
                        end
                end
            end,
            __index=function(o,k) 
                    return R(rawget(o,k) or EF("no item named '%s' in %s",k,n))
            end
        })
    end

    local function err_no_key(t,n) -- I "bless" tables so that they will fail if an unexistent index is looked for
        n = n or E("err_no_key without name")
        return setmetatable(t,{
            __index=function(o,k) 
                    return rawget(o,k) or EF("no item named '%s' in %s",k,n)
            end
        })
    end
    -- the collections
    local types = err_no_key({},"Types"); -- holds all named types (classes, enums, callbacks, functions)
    local prototypes = dup_to({},"known prototypes") -- I hold the collection of prototypes (by name)
    local classes = dup_to(types,"Classes") -- I hold the collection of classes (by name)
    local functions = dup_to(types,"Functions") -- I hold the collection of functions that are not in a class (by name)
    local enums = dup_to(types,"Enums") -- I hold the enumerations (by name) -- nothing for now
    local callbacks = dup_to(types,"CallBacks") -- I hold the callbacks (by name)
    local values = dup_to(types,"Values") -- I hold Lua values to be added (by name) 
    
    local test -- I'm a dummy for: test = cond or error()
    
    -- utility stuff
    
    function string.mcount(s,pat) -- I count occurrences of pat in a string
        local ln = 0

        for m in s:gmatch(pat) do
            ln = ln + 1
        end
        
        return ln
    end
    
    function string.split(s,p,pm,pq) -- I somehow split strings
        local t = {}
        for i in (s..(pm or p)):gmatch( f("([^%s]*)%s%s",p,p,(pq or '')) ) do
            table.insert(t,i)
        end
        return t
    end
    
    function string.chomp(s)
        local _s = s:match("^%s*(.*)")
        _s = _s:reverse()
        _s = _s:match("^%s*(.*)")
        _s = _s:reverse()
        return _s;
    end
    
    string.C = string.chomp
    
    local function fmt_o(s,o) -- I replace %{xxx} with o[xxx] even %{xxx.yyy.zzz} with o[xxx][yyy][zzz]
        local function replace(m)
            if m:match("[.]") then
                local r = o
                
                for i,el in ipairs(m:split(".",".")) do 
                    r = r[el] or EF("No such element in table: '%s'",m)
                end
                return r
            else
                return o[m]
            end
        end
        return s:gsub("%%[{]([%w_.-]+)[}]", replace)
    end

    local F = fmt_o
    string.F = fmt_o

    local function xor(a,b) return (a and not b) or (b and not a) end
    
    local function v2s(t,i,_s) -- I dump stuff into a string 
        i = i or "";
        local oi = i
        local s = i
        local tt = type(t)
        
        if     tt== 'string' then
            return '"' .. t .. '"' 
        elseif tt== 'number' then
            return tostring(t)
        elseif tt== 'table' then
            s = s .."{ -- " ..tostring(t).."\n";
            i = "  " .. i

            for k,v in pairs(t) do
                local tv = type(v)
                local tk = type(k)

                s = s .. i 

                if tk == 'string' then
                    s = s .. "'"..k.."' ="
                else
                    s = s .. k .." ="
                end

                if tv == 'table' then
                    --_s = _s or {};
                    --for k,v in ipairs(_s) do
                    --    if t == v then error("circular reference") end
                    --end
                    --table.insert(_s,t)
                    s = s .. v2s(v,i,_s)
                    --table.remove(_s,t)
                elseif tv == 'string' then
                    s = s .. '"' .. v .. '"'
                else
                    s = s .. tostring(v)
                end
                s = s .. ",\n"
            end
        else
            return '[['..tostring(t)..']]'
        end
        return s .. oi .. "}"
    end

    
    DR = function()
        local s = ''
        
        for _i, r in pairs(_R) do
            s = s .. "\n" .. v2s(r) .. "\n"
        end
        
        return s
    end
    
    -- LW tools:
    local dump = print
    
    local function log(level, ...) -- I dump objects based on verbosity
      local s = ''
      if (tonumber(level) <= tonumber(verbose)) then
        for i,a in ipairs({...}) do s = s .. v2s(a) .. "  " end
      end
      dump(s)
    end
    
    local function D(o) log(0,o) end

    local function output(f,cmd) -- I  manage an output file adding #line directives when needed
        -- return 3 functions:
        --    add(lines,filename,linenum)
        --    vadd(v_level,lines,filename,linenum)
        --    add(lines,filename,linenum)
        local ff = string.f
        local s = cmd and ff('/* %s: Generated on %s by: "%s" */\n',f,os.date(),cmd); -- the output string
        local l = 3; -- the line number in the output
        local last_f = f -- the last file input was added 
            local function add_fn(_s,_f,_l) -- adds _s to the file from _f:_l
                _f = _f or f -- no source means is unsourced generated output (so we point to the output)
                _l = _l or l
            
                --s =s.."{"..last_f.."}"
                --s =s.."{".._f.."}{".._l.."}"
                local __f
                local __l
                
                if (_f == last_f) then
                    __f = nil
                    __l = _l
                else
                    last_f = _f
                    __f = _f
                    __l = _l
                end
                
                local __s = (__f and ff('#line %d "%s"\n',__l,__f) or '') .. _s
                
                __l = __s:mcount("\n")
                l = l + __l
                s = s .. __s
                return __l
            end
        local function write_fn() -- writes the file as it is
                local fd = io.open(f,'w') or EF("cannot open '%s' for writing",f)
                test = fd:write(s) or EF("cannot write to '%s'",f)
                fd:close()
                return l
            end
        local function vadd_fn(_v,_s,_f,_l)
            return (verbose < _v) and add_fn(_s,_v,_f,_l) or 0
        end
        return add_fn, vadd_fn, write_fn
    end
    
    local function reader(filename,add,vadd,finish) -- I create a file reader
        local f = io.open(filename, "r");
        
        if not f then
            EF("Cannot open file: '%s'", filename)
        end

        local s = f:read("*all")
        f:close()
        
        
        local fr = { finish=finish,  ln=0, filename=filename, add=add, vadd=vadd  }
        local it =s:gmatch("([^\n]*)[\n]?")
        function fr.iter()
            fr.ln = fr.ln+1;
            return it()
        end
        
        return fr
    end

    -- C code generators
    
    local function proto2C_call(m) -- given a method I yield the call code
        local s = F("%{proto.name}(",m)

        for i,a in pairs(m.proto.argv) do
            s = s ..F("%{name}, ",a)
        end
        
        if #m.proto.argv > 0 then
            s = s:sub(1,-3)
        end
        return s .. ");"
    end
    
    
    local idx_arg
    
    local farg_t2C = { -- function arguments for each Ts
        N=function(a) 
            idx_arg = idx_arg + 1;
            a.idx = idx_arg;
            test = a.name or EF("N-argument[%d] without a name",a.idx);
            local decl, input
            
            decl = a.type and F("  %{type} %{name} ;/* arg[%{idx}] %{text} */\n",a) or ""
            
            if type(a.default) == "string" and a.default ~= "" then
                input = F("  %{name}  = optN(L, %{idx}, %{type}, %{default}); /* arg[%{idx}] %{text} */\n",a)
            else
                input = F("  %{name} = checkN(L,%{idx},%{type}); /* arg[%{idx}] %{text} */\n",a)
            end
            
            return decl, input
        end,
        B=function(a) 
            idx_arg = idx_arg + 1;
            a.idx = idx_arg;
            local decl, input
            test = a.name or EF("B-argument[%d] without a name",a.idx)
            
            decl = F("  int %{name}; /* arg[%{idx}] %{text} */",a)
            
            if a.default and a.default ~= "" then
                input = F("  %{name} = optB(L, %{idx}, (int)%{default}); /* arg[%{idx}] %{text} */\n",a)
            else
                input = F("  %{name} = checkB(L, %{idx}); /* arg[%{idx}] %{text} */\n",a)
            end
            return decl, input
        end,
        S=function(a)
            idx_arg = idx_arg + 1;
            a.idx = idx_arg;
            local decl, input
            test = a.name or EF("S-argument[%d] without a name",a.idx);
            decl = F("  const char* %{name}; /* arg[%{idx}] %{text} */\n",a)
            if a.default and a.default ~= '' then
                input = F("  %{name} = optS(L, %{idx},%{default}); /* arg[%{idx}] %{text} */\n",a)
            else
                input = F("  %{name} = checkS(L, %{idx}); /* arg[%{idx}] %{text} */\n",a)
            end
            return decl, input
        end,
        L=function(a)
           idx_arg = idx_arg + 1;
           a.idx = idx_arg;
           local decl, input
            test = a.name or EF("L-argument[%d] without a name",a.idx);
            test = a.len_name or EF("L-argument[%d] '%s' without len_name",a.idx,a.name)
            a.type = a.type or "const char*";
            a.len_type = a.len_type or 'size_t';
            
            decl  = F('  %{len_type} %{len_name}; %{type} %{name}; /* arg[%{idx}] %{text} */\n',a)
            input = F('  %{name} = checkL(L, %{idx}, &%{len_name}); /* arg[%{idx}] %{text} */\n',a)
            return decl, input
        end,
        X=function(a)
            local decl, input
            test = a.name or EF("X-argument[%d] without a name",a.idx);
            test = a.type or EF("X-argument[%d] '%s' without a type",a.idx,a.name);
            test = a.value or EF("X-argument[%d] '%s' without a value",a.idx,a.name);
            
            decl =  F("  %{type} %{name};  /* X-arg: %{text} */\n",a)
            input = F("  %{name} = (%{type})(%{value});  /* X-arg: %{text} */\n",a)
            return decl, input
        end,
        O=function(a) 
           idx_arg = idx_arg + 1;
           a.idx = idx_arg;
           local decl, input
            test = a.name or EF("%s-argument[%d] '%s' without a name",a.type,a.idx);
            decl = F("  %{type}* %{name};  /* arg[%{idx}] %{text} */\n",a)
            
           if a.default and a.default ~= '' then
                input =  F("  %{name} = opt%{type}(L, %{idx}, %{default});  /* arg[%{idx}] %{text} */\n",a)
            else
                input =  F("  %{name} = check%{type}(L, %{idx});  /* arg[%{idx}] %{text} */\n",a)
            end 
            return decl, input
        end,
        F=function(a)
           idx_arg = idx_arg + 1;
           a.idx = idx_arg;
            local decl, input
            
            local cb = callbacks[a.type] or EF("No such Callback '%s'",a.type);
            decl = F("  %{type} %{name};  /* arg[%{idx}] %{text} */\n",a)
            
            if cb.mode == 'key' then
                test = a.key_name or EF("Keyed Callback '%s' without key_name",a.type)
                
                input = F("  %{name} = check%{type}(L, %{idx}, %{key_name});  /* arg[%{idx}] %{text} */\n",a)
            else
                input = F("  %{name} = check%{type}(L, %{idx});  /* arg[%{idx}] %{text} */\n",a)
            end
            return decl, input
        end,
    }
            
    local fret_t2C = { -- function returs for Ts
        N=function(r,m)
            idx_ret = idx_ret +1
            r.idx = idx_ret
            test = r.name or EF("N-return[%d] without a name",r.idx);
            r.type = r.type or "lua_Number"
            return F("  %{type} %{name}; /* ret[%{idx}] %{text} */\n",r),
                F("  pushN(L,%{name});  /* ret[%{idx}] %{text} */\n",r)
        end,
        S=function(r,m)
            idx_ret = idx_ret +1
            r.idx = idx_ret
            test = r.name or EF("S-return[%d] without a name",r.idx);
            return F("  const char* %{name}; /* ret[%{idx}] %{text} */\n",r),
                F("  pushS(L,%{name});  /* ret[%{idx}] %{text} */\n",r)
        end,
        B=function(r,m)
            idx_ret = idx_ret +1
            r.idx = idx_ret
            test = r.name or EF("B-return[%d] without a name",r.idx);
            return F("  int %{name}; /* ret[%{idx}] %{text} */\n",r),
                F("  pushB(L,%{name});  /* ret[%{idx}] %{text} */\n",r)
        end,
        L=function(r,m)
            idx_ret = idx_ret +1
            r.idx = idx_ret
            test = r.name or E("L-return without a name");
            test = r.len_name or EF("L-return '%s' without len_name",r.name)
            r.len_type = r.len_type or 'size_t';
            r.bufsize = r.bufsize or 65536;
            
            local len = r.len_name ~= '_'
                and F("%{len_type} %{len_name} = %{bufsize};  /* ret[%{idx}] %{text} */\n",r)
                or ''
            
            return  F("  char %{name}[%{bufsize}];  /* ret[%{idx}] %{text} */\n",r) .. len,
                    F("  pushL(L, %{name}, %{len_name});  /* ret[%{idx}] %{text} */\n",r)
        end,
        O=function(r,m)
            idx_ret = idx_ret +1
            r.idx = idx_ret
            test = r.name or EF("%s-return[%d] without a name",r.type,r.idx);
            return F("  %{type}* %{name};  /* ret[%{idx}] %{text} */\n",r),
                F("  push%{type}(L, %{name});  /* ret[%{idx}] %{text} */\n",r)
        end,
        X=function(r,m)
            test = r.name or EF("X-return[%d] without a name",r.idx);
            test = r.type or EF("X-return[%d] '%s' without a type",r.idx,r.name);
            test = r.value or EF("X-return[%d] '%s' without a value",r.idx,r.name);

            local s1 = r.type and F("  %{type} %{name};\n",r) or ""
            local s2 = F("  %{name} = (%{type})(%{value});  /* ret[%{idx}] %{text} */\n",r)
            return s1, s2
                
        end;
    }
    
    local function function2C(m) -- given a method I yield C code for it
        test = m.name or E("function without a name");
        test = m.proto or EF("function '%s' without a prototype",m.name);
        
        local hdr = F("// calling: %{proto.line}\nstatic int %{codename}(lua_State* L) {\n",m)
        
        local decls = ''
        local input = ''
        local call = ''
        local output = ''
        
        idx_arg = 0
        for idx,arg in pairs(m.args) do
            local _d , _i =  farg_t2C[arg.t](arg,m)
            R(_d)
            R(_i)
            decls = decls .. ( arg.name:M("^%s*[%w_]+%s*$") and _d or "" )
            input = input .. _i
        end
        
        local done_wit_ret = false
        
        idx_ret = 0
        for idx,ret in pairs(m.rets) do
            local _d, _o = fret_t2C[ret.t](ret,m)
            if ret.name == '_' then done_wit_ret = true end
            decls = decls .. ( not ret.name:M("^[%w+]+$") and _d or "" )
            output = output .. _o
        end
        
        if (m.proto.type ~= 'void') then 
            decls = decls .. (done_wit_ret and "" or f("  %s _;\n", m.proto.type))
            input = input .. f("\n  _ = %s\n",proto2C_call(m))
        else
            input = input .. f("  %s\n",proto2C_call(m))
        end

        return R(f("%s\n%s\n%s\n%s  return %d;\n}\n",hdr,decls,input,output,#m.rets))
    end
    
    local function accessor2C(a) -- given a method I yield C code for it
        test = a.name or E("function without a name");
        local s = F("static int %{cname}_%{name}(lua_State* L) {\n %{cname}* __o = check%{cname}(L,1);\n",a)

        if a.type == 'RW' then 
            s = s .. ' if (lua_gettop(L)>1) {\n'
            for name,_in in pairs(a.ins) do
                local function SNB(ar) return (" (%{expr}) = check%{t}(L,%{idx});"):F(ar) end
                s = s .. (({
                    L=function(ar) return (" (%{expr}) = checkL(L,%{idx},&(%{len_expr}));"):F(ar) end,
                    O=function(ar) return (" (%{expr}) = check%{type}(L,%{idx}));"):F(ar) end,
                    S=SNB,N=SNB,B=SNB
                })[_in.t](_in))
            end
            s = s .. ' } else {\n'
        end
        
        
        for idx,_out in pairs(a.outs) do
            local function SNB(ar) return (" push%{t}(L,(%{expr})); /* %{text} */\n"):F(ar) end
            s = s .. (({
                L=function(ar) return (" pushL(L,(%{expr}),&(%{len_expr})); /* %{text} */\n"):F(ar) end,
                O=function(ar) return (" push%{type}(L,(%{expr}))); /* %{text} */\n"):F(ar) end,
                S=SNB,N=SNB,B=SNB
            })[_out.t](_out))
        end

        if a.type == 'RW' then 
            s = s .. '}'
        end
        
        local sout = f("%s  return %d;\n}\n",s,#a.outs)
        return sout:gsub("[$]","__o->")
    end
    

    local function class2C(c) -- given a class I yield C code for its basic functions or declarations
        return F("typedef %{type} %{name};\n"..((c.own and "LqwDeclareClass(%{name});\n") or "LqwDefineClass(%{name});\n"),c)
    end

    local function registration2C(fn_name) -- I yield C code for registration to Lua of classes and functions 
        local s = f("extern int %s(lua_State* L) {\n",fn_name)
        local b =  "  lua_newtable(L);\n"
        for n,c in pairs(classes) do
        R(c)
            s = s .. F("  lual_Reg* %{name}_methods = {\n",c)
            for name, m in pairs(c.methods) do
                R(m)
                test  = m.name and m.codename or EF("Function without names")
                s = s .. F('    {"%{name}",%{codename}},\n',m)
                IR(m)
            end
            s = s .. "    {NULL,NULL}};\n"
                        
            s = s .. F("  lual_Reg* %{name}_metas = {\n",c)
            for name, m in pairs(c.meta) do
                R(m)
                test  = m.name and m.codename or EF("Function without names")
                s = s .. F('    {"%{name}",%{codename}},\n',m)
                IR(m)
            end
            s = s .. F('    {"__gc",%{name}___gc},\n    {NULL,NULL}};\n',c)
            
            b = b .. F('  LqwClassRegister(%{name});\n',c)

            for name, v in pairs(c.values) do
                R(v)
                b = b .. F('  push%{t}(L,(%{expr}));\n  lua_setfield(L,-2,"%{name}");\n',v)
                IR(v)
            end
        IR(c)
        end
        
        s = s .. "  lual_Reg* functions = {\n"
        for name, f in pairs(functions) do
            R(f)
            test  = f.name and f.codename or EF("Function without names")
            s = s .. F('    {"%{name}",%{codename}},\n',f)
            IR(f)
        end
        s = s .. "    {NULL,NULL}};\n  lual_Reg* f;\n\n"
        b = b .. "  \n  for (f=functions;f->name;f++) {\n" ..
                 '    lua_pushcfunction(L,f->func); lua_setfield(L,1,f->name);\n  }\n\n'
        
        for name, v in pairs(values) do
            R(v)
            b = b .. F('  push%{t}(L,(%{expr}));\n  lua_setfield(L,1,"%{name}");\n',v)
            IR(v)
        end
        
        local cb = '  /* Register C Callbacks */\n  lua_newtable(L);\n'
            
        for tname, cbf in pairs(callbacks) do
            R(cbf)
            cb = cb .. "   lua_newtable (L);\n"
            for name, ccb in pairs(cbf.c_cbs) do
                R(ccb)
                cb = cb .. f('     lwcRegCB(L, -1, "%s", %s,"%s");\n',name,ccb.fn_name,ccb.tname);
                IR(ccb)
            end
            cb = cb .. f('    lua_setfield(L,1,"%s");\n',tname);
            IR(cbf)
        end
        cb = cb .. '  lua_setfield(L,1,"_CallBacks")\n'
        
        return R(s .. b .. cb .. "  return 1;\n}\n")
    end
    
    local a_idx

    local cb_arg_t = {
        B=function(a) a_idx = a_idx + 1;  a.idx = a_idx; return F("  pushB(L,(int)%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        N=function(a) a_idx = a_idx + 1;  a.idx = a_idx; return F("  pushN(L,(lua_Number)%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        O=function(a) a_idx = a_idx + 1;  a.idx = a_idx; return F("  push%{type}(L,%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        F=function(a) a_idx = a_idx + 1;  a.idx = a_idx; return F("  push%{type}(L,%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        S=function(a) a_idx = a_idx + 1;  a.idx = a_idx; return F("  pushS(L,%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        L=function(a)
            a_idx = a_idx + 1;  a.idx = a_idx; 
            return F("  pushL(L,%{name},(size_t)%{len_name}); /* arg[%{idx}] %{text} */\n",a)
        end,
        X=function(a) 
            test = a.name or EF("X-argument[%d] without a name", a.idx);
            test = a.type or EF("X-argument[%d] '%s' without a type", a.idx, a.name);
            test = a.value or EF("X-argument[%d] '%s' without a value", a.idx, a.name);

            return F("  %{name} = (%{type})%{value}; /* %{text} */\n",a)
        end,
        U= function(a) 
            test = a.name or EF("U-argument[%d] without a name", a.idx);

            return F("  (void)%{name}; /* %{text} */\n",a)
        end
    }
    
    local r_idx
    
    local cb_ret_t = {
        B=function(a)
            r_idx = r_idx + 1;
            a.idx = r_idx;
            return a.name:M('^[%a][%w_]*$') and F("  int %{name};  /* ret[%{idx}] %{text} */\n",a) or "",
                F("    %{name} = toB(L,%{idx}); /* ret[%{idx}] %{text} */\n",a)
        end,
        N=function(a,m)
            r_idx = r_idx + 1;
            a.idx = r_idx;
            if a.name == '_' then 
                a.type = a.type or m.proto.type
            else
                a.type = a.type or "lua_Number"            
            end
            
            return a.name:M('^[%a][%w_]*$') and F("  %{type} %{name};  /* ret[%{idx}] %{text} */\n",a) or "",
                F("    %{name} = toN(L,%{idx},%{type}); /* ret[%{idx}] %{text} */\n",a)
        end,
        O=function(a)
            r_idx = r_idx + 1;
            a.idx = r_idx;
            return a.name:M('^[%a][%w_]*$') and F("  %{type}* %{name};  /* ret[%{idx}] %{text} */\n",a) or "",
                F("    %{name} = to%{type}(L,%{idx}); /* ret[%{idx}] %{text} */\n",a)
        end,
        S=function(a)
            r_idx = r_idx + 1;
            a.idx = r_idx;
            return a.name:M('^[%a][%w_]*$') and F("  const char* %{name};  /* ret[%{idx}] %{text} */\n",a) or "",
                F("    %{name} = toS(L,%{idx}); /* ret[%{idx}] %{text} */\n",a)
        end,
        L=function(a)
            r_idx = r_idx + 1;
            a.idx = r_idx;
            local len_def, var_def
            a.len_type = a.len_type or 'size_t';
            a.bufsize = a.bufsize or 65536;

            if (not a.len_name) or a.len_name:M('^[%a][%w_]*$') then
                len_def = '';
            else
                len_def =  F("%{len_type} %{len_name}; ",a)
            end
                        
            return len_def .. F("  /* ret[%{idx}] %{text} */\n",a),
                F("    %{name} = toL(L,%{idx},&(%{len_name})); /* ret[%{idx}] %{text} */\n",a)
        end,
    }
    

    local function cb2C(c) -- given a callback I yield C code for it
        local start = "typedef %{name} %{proto.name};\n"
        local call =  '  int __res;\n'

        local check = '#define check%{name}(L, idx'

        if c.mode == 'closure' then
            start = start .. "static int %{name}__fn_idx;\n"
            check = check .. ") "
            call = call  .. '  lua_replace(L, %{name}__fn_idx );\n\n'
        else
            call = call  ..  '  int __fn_idx = lua_gettop(L)+1;\n'
                .. '  const char* __key = cbKey%{name}((void*)(%{key_name}));\n'
              .. '\n  lua_getfield(L, LUA_REGISTRYINDEX, __key );\n\n'
            
            check = check .. ", F) "
        end

        if c.mode == 'closure' then
            check = check .. '  (%{name}__fn_idx = idx, checkCB(L, idx, call%{name}, NULL, NULL, "%{name}"))\n\n'
        else 
            check = check .. '  (checkCB(L, idx, call%{name}, cbKey%{name}, %{key_name}, "%{name}"))\n\n'
        end
        
        start = start .. "LQW_MODE %{proto.type} call%{name}(%{proto.arglist}) {\n  luaState* L = lwState();"
                      .. (c.proto.type ~= 'void' and "\n  %{proto.type} _ = %{ret_def};\n" or "")
        a_idx = 0;
        
        for idx, a in ipairs(c.args) do
            local c = cb_arg_t[a.t] or function(a) return "" end
            call = call .. c(a,c);
        end

        r_idx = 0;
        local call_body = '' 
        for idx,r in ipairs(c.rets) do
            local cb = cb_ret_t[r.t] or EF("return %{name} is of invalid type '%{t}'",r);
            local decls, body = cb(r,c);
            
            start = start .. decls
            call_body = call_body .. body
        end
                
        call = call .. f("\n  if((__res = lua_pcall(L, %d, %d, 0})== LUA_OK)){\n",a_idx,#c.rets)
        

        call  = call .. call_body .. f("    lua_pop(L,%d)\n",#c.rets)
        
        if c.proto.type ~= 'void' then
            call = call .. "    return _;\n"
        else
            call = call .. "    return;\n"
        end
        
        if c.mode == 'closure' then
            call = call .. '  } else lqwPcallErr(NULL,__res);\n\n'
        else 
            call = call .. '  } else lqwPcallErr(__key,__res);\n\n'
        end

        if c.proto.type ~= 'void' then
            call = call .. "  return _;\n}\n"
        else
            call = call .. "  return;\n}\n"
        end
        
                
        return  F(start .. call ..check, c);
    end
    
    
    local function splitParams(sep,params,names,ps)
        -- I split a separated list, and assign its elements to an object based on names
        --log(5,'<splitParams',params,names,ps)
        local i = 1
        ps = ps or {}
        for p in (params..sep):gmatch("%s*([^"..sep.."]*)%s*" .. sep) do
            local name = names[i]
            i = i + 1
            if name then ps[name] = p else break end
        end
        --log(5,'splitParams>',v2s(ps))
        return ps, i
    end

    local function splitList(list,t_pars,tbl)
        --log(5,'<splitList',list,v2s(t_pars),v2s(tbl))
        tbl = tbl or {}
        local i = 0
        for ty, par in list:gmatch("(%u[%w]*)[{]%s*([^}]*)%s*[}]") do
            i = i + 1
            local r = {idx = i, text = f("%s{%s}",ty,par)}
            local t_par = t_pars[ty]
            
            if t_par then
                r.t = ty
            else
                local t = types[ty].t
                r.t = t
                r.type = ty
                t_par = t_pars[t] or EF("No params for type '%s'",t)
            end
            
            splitParams(";",par,t_par,r)
            table.insert(tbl,r)
        end
        
        --log(5,'splitList>',v2s(rets))
        return tbl, i
    end


    function lqw_cmd.Module(params,val,frame)
        if module_name then E("module already defined") end        
        log(2,'Module:',params)
        local p = params:split("%s"," ","+");
        module_name = p[1]:M("^(%u%w+)$") or E("no module name given")
        mode = M("^(%a+)$",p[1]) or "static"
        test = mode:M("static") or E("only static mode supported")
        local s = "#include <stdio.h>\n#include <lua.h>\n#include <lualib.h>\n#include <lauxlib.h>\n"
          .. f("#define LQW_MODULE %s\n#define LQW_MODE %s\n",module_name,mode)
        frame.add(s)
        
        
        if mode == 'static' then
            local fn = own_prefix .. lqw_include
            local fd = io.open(fn,"r") or E("cannot open: " .. fn)
            frame.add(fd:read("*all") or E("cannot read: " .. fn),fn,1)
            fd:close()


            fn = own_prefix .. lqw_code
            fd = io.open(fn,"r") or E("cannot open: " .. fn)
            frame.add(fd:read("*all") or E("cannot read: " .. fn),fn,1)
            fd:close()
        else -- 'extern'
            -- copy luawrap.h
            -- copy luawrap.c
            frame.add('#include "luawrap.h\n')
        end
        
        log(3,'Module:',module_name,mode)
    end

    
    local function class (params,val,frame)
        local c = {filename=frame.filename,ln=frame.ln}
        local p = params:split("%s"," ","+");
        
        c.name = p[1]:match("^([%w_]+)$") or E("Class must have a name [A-Z][A-Za-z0-9_]+")
        c.type = p[2]:match("^([%w_]+)$") or E("Class must have a type [A-Za-z0-9]+")
        
        c.methods=dup_to({},f("%s_methods",c.name))
        c.meta = dup_to({},f("%s_meta",c.name))
        c.values=dup_to({},f("%s_values",c.name))
        
        c.t = 'O';
        classes[c.name] = c

        return c
    end
    
    function lqw_cmd.Class(params,val,frame)
        local c = class(params,val,frame)
        frame.add(class2C(c));
    end
    
    function lqw_cmd.OwnClass(params,val,frame)
        local c = class(params,val,frame)
        c.own = true
        frame.add(class2C(c));
    end

    function lqw_cmd.Alias(params,val,frame)
        local dc,dn,sc,sn = params:M("^%s*(%u[%w_]+)[.]?([%w_]+)%s+(%u[%w_]+)[.]([%w_]+)%s*$")
        test = dc and dn and sn and sc or E("Malformed Alias")
        local d = ( dc =='' and functions ) or classes[dc][dn:M('^__') and 'meta' or 'methods']
        local s = ( sc =='' and functions ) or classes[sc][sn:M('^__') and 'meta' or 'methods']
    
    end

    function lqw_cmd.Accessor(params,val,frame)        
        local acc_t = {
            L={"expr","len_expr"},
            N={"expr"},   
            S={"expr"},
            B={"expr"},
            O={"expr"},
        }
        
        local a = R({})
        a.cname, a.name, a.type = params:M("^%s*(%u[%w]+)[.]([%w_]+)%s+(R[WO])%s*$")
        test = a.type or EF("Accessor malformed")
        test = a.name and a.name ~= '' or EF("Accessor missing name")
        
        local c = classes[a.cname] or EF("No such class '%s'",a.cname)
        a.codename = f("%s_%s",a.cname,a.name)
        
        if a.type == 'RO' then
            local outs = val:M("^%s*(.*%S+)%s*$")
            a.outs = splitList(outs,acc_t)
            a.ins = {}
        else 
            local ins,outs = val:M("^%s*([^@]*)%s*[@]?%s*(.*)%s*$")
            a.ins = splitList(ins,acc_t)
            a.outs = splitList(outs,acc_t)
        end
        
        
        
        c[a.name:M("^__") and 'meta' or 'methods'][a.name] = a
        
        frame.add(accessor2C(a))
        
    end
    
    function lqw_cmd.OwnFunction(params,val,frame)
        local cname, name = params:M("^%s*(%u[%w]+)[.]([%w_]+)%s*$")

        if not cname then
            name = params:M("^%s*([%w_]+)%s*$") or EF("Didn't get a valid name")
            functions[name] = {
                fullname=name, name=name, codename=f("%s_%s",module_name,name),
                own=true, filename=frame.filename, ln=frame.ln
            }
        else
            local c = classes[cname]
            local cont = name:M("^__") and c.meta or c.methods
            cont[name] = {
                fullname=f("%s.%s",cname,name),cname=cname, name=name, codename=f("%s_%s",cname,name),
                own=true, filename=frame.filename, ln=frame.ln
            }
        end
    end

    function lqw_cmd.OwnCallBack(params,val,frame)
        local m={own=true, filename=frame.filename, ln=frame.ln, is_cb=true, t='F' }
        R(m)
        R({params=params,val=val})

        m.name, m.key_name, m.typedef = params:M("^%s*(%u[%w_]+)[{]?([%w_]*)[}]?%s+([%w_]+)%s*$")
            
        test = m.name or EF("CallBack %s has no good name",params);
        
        m.c_cbs=dup_to({},'%s_ccbs' .. m.name)
        
        mode = (m.key_name and m.key_name ~= '' and 'key') or 'closure'
            
        callbacks[m.name] = m
    end

    local finished= false
    
    function lqw_cmd.Finish(params,val,frame)
--        R(types)
--        R(prototypes)
        local fname = params:M("^%s*([%g_]+)%s*$") or E("Not a valid registration function name");
        local s = registration2C(fname);
        frame.add(s);
        finished = true
    end
    
    function lqw_cmd.Break(params,val,frame)
        local ln, file = params:match('^%s*([0-9]+)%s*["]?([^"]*)["]?%s*$')
        test = ln or E("Malformed Break")
        
        debug_linebreak = tonumber(ln)
        debug_filebreak = file == '' and frame.filename or file
    end

    local function shift(t) return table.remove(t,1) end

    local arg_t_opts = { -- options for function arguments part
        N={"name","type","default"},
        S={"name","default"},
        B={"name","default"},
        L={"name","len_name","len_type"},
        O={"name","default"},
        X={"name","value","type"},
        F={"name","key_name"},
        U={"name"}
    }
    
    local ret_t_pars = { -- options for return argument parts
        N={"name","default"},
        S={"name","default"},
        B={"name","default"},
        L={"name","len_name","bufsize","len_type","len_def","default"},
        O={"name","ref","default"},
        X={"name","value","type"},
    }
    
    function lqw_cmd.Function(params,val,frame,m)
        
        log(2,'Function:',params,val)
        local m = m or {
            filename=frame.filename, ln=frame.ln,
            params=params, expr=val, rets={}, args={}
        }
        
        R(m)
    
        local pars = params:split("%s"," ",'+');
        m.fullname = shift(pars) or E("Function/Callback must have name parameter")
        m.fn_name = shift(pars) or E("Function/Callback must have fn_name parameter")
        m.proto = prototypes[m.fn_name]
        
        
        local collection
        if m.is_cb then
            test = m.proto.typedef or EF("Callback prototype must be a callback prototype");
            m.ret_def = shift(pars)
            
            m.name, m.key_name = m.fullname:match("%s*(%u[%w_]+)[{]?([%w_]*)[}]?%s*")
            
            test = m.name or EF("CallBack %s has no good name",m.fullname);
            
            m.mode = m.key_name ~= '' and 'key' or 'closure'
            
            if m.proto.type ~= 'void' and not m.ret_def then
                EF("Callback '%s' has non void return type but was given no return default.",m.name)    
            end
            
            collection = callbacks
            m.c_cbs=dup_to({},f("%s_ccbs",m.fullname))

        else
            if m.fullname:match("%s*(%u[%w]+)[.]([%w_]+)%s*") then
                m.cname, m.name = m.fullname:match("(%u[%w]+)[.]([%w_]+)")
                m.codename = m.cname .. "_" .. m.name
                m.is_meta = m.name:M("^__") and true or false
                local class = classes[m.cname]

                collection = class[m.is_meta and "meta" or "methods"]
            elseif m.fullname:match("([%w_]+)") then
                m.name = m.fullname
                m.codename = module_name .. "_" .. m.name
                m.is_orphan = true;
                collection = functions
            else
                E("Not a good name: "..v2s(m.fullname))
            end
        end

        m.names=dup_to({},f("%s_names",m.fullname))

        local i = 0
        local ret, args = val:match("%s*([^@]*)%s*[@]?([^@]*)%s*")
        
        test = (ret and args) or E("malformed method:" .. v2s(m.fn_name) )
        
        splitList(ret,ret_t_pars,m.rets)
        splitList(args,arg_t_opts,m.args)
        
        for k,a in pairs(m.args) do
            if not a.type and a.t ~= 'K' and a.t ~= 'F' then
                if m.proto.args[a.name] then
                    a.type = m.proto.args[a.name].type
                else 
                    if a.t == 'S' then
                        a.type = 'const char*'
                    elseif a.t == 'N' then
                        a.type = 'lua_Number'
                    elseif a.t == 'U' then
                        a.type = 'void'
                    else
                        EF("In %s: argument '%s',is missing type (and is not in prototype):\n  %s",
                            m.fullname,a.name,m.proto.line)
                    end
                end
            end
        end
                
        collection[m.name] = m
        local s = (m.is_cb and cb2C or function2C)(m)
        frame.add(s)
        log(3,"Function",m)
    end

    function lqw_cmd.CallBack(params,val,frame)
        
        local cb = lqw_cmd.Function(params,val,frame,{
            is_cb=true,filename=frame.filename, ln=frame.ln,
            rets={}, args={}, params=params, expr=val, t='F'
        })
        
    end
    
    
    function lqw_cmd.Value(params,val,frame)
        local v = R({})
        v.cname, v.name, v.t = params:M("^(%u?[%w]*)[.]?([%w_]+)%s+(%u[%w_]*)$")
        v.expr = val:C();
        
        test = v.t:M("^[NSB]$") or classes[v.t]  
        
        
        if (v.cname) then
            v.fullname = v.cname .. '.' .. v.name
            local c = classes[v.cname]
            c.values[v.name] = v
        else 
            v.fullname = v.name
            values[v.name] = v
        end

    end
    

    function lqw_cmd.CCallBack(params,val,frame)
        local m={own=true, filename=frame.filename, ln=frame.ln, is_cb=true, t='F',c}
        R(m)
        m.tname, m.name, m.fn_name = params:match("^%s*(%u[%w]+)%s+([%w_]+)%s+([%w+]+)%s*$")
        test = m.tname or E("Invalid CallBack name!")
        test = m.name or E("Invalid name!")
        test = m.fn_name or E("Invalid fn_name!")
        
        m.proto = prototypes[m.fn_name]

        test = m.proto.typedef and EF("Prototype '%s' must be that of a function.",m.fn_name);
                    
        mode = (m.key_name and m.key_name ~= '' and 'key') or 'closure'
            
        local c = callbacks[m.tname]
        c.c_cbs[m.name] = m;
        
    end
    
    local function lwcmd(lwc,frame,stack)
        NRA()
        local cmd,params,val = lwc:match("(%u[%w]+)%s*([^:]*)[:]?%s*(.*)%s*")
        R({lwc=lwc,frame=frame})
        
        local cmt = f("// %s:%d #%s\n",frame.filename,frame.ln, lwc)
        frame.add(cmt)
        if cmd ~= 'Module' and not module_name then E("no module defined") end
        if finished then E("no commands after Finish.") end

        if cmd == 'ProcFile' then
            local infile, outfile = lwc:match('ProcFile%s+([^%s]+)%s*([^%s]*)%s*')
            
            test =  not infile and E("No input file in ProcFile directive")
            local new_fr = reader(infile,frame.add,frame.vadd,frame.finish)
            table.insert(stack,new_fr)
            frame = stack[#stack]
            if outfile then
                if mode == 'extern' then
                    frame.add, frame.vadd, frame.finish = output(outfile)
                else
                    E("Cannot use ProcFile in 'static' mode");
                end
            end
            return frame 
        elseif cmd then
            if lqw_cmd[cmd] then
                lqw_cmd[cmd](params,val,frame)                
                return frame;
            else
                EF("No such command: '%s'", cmd)
            end
        else
            EF(" bad cmd: '%s'",lwc)
        end
    end
    
        
        
    local function process(filename,add,vadd,finish)
        local stack = {reader(filename,add,vadd,finish)};
        stack[1].base = true;
        
        while #stack > 0 do
            local frame = stack[#stack]
            local ml_lwc
            
            while true do
                local line = frame.iter()
                if not line then break end
                
                frame.cur_line = line

                if mode=='static' and frame.base then
                    if not (ml_lwc or line:match("^[#][%u]")) then
                        frame.add(line.."\n",frame.filename,frame.ln)
                    end
                end
            
                local function _err(str)
                    local s = f('%s:%d: error: "%s"\nLine:%s\n',frame.filename,frame.ln,str,v2s(line))
                                .. debug.traceback() .. "\n"
                    if verbose > 0 then
                        s = s .. DR()
                    end
                    
                    log(0,s)
                    add(f('#error "%s" ',str),frame.filename,frame.ln);
                    finish();
                    io.stderr:write(s)
                    
                    os.exit(3)
                end
            
                E = _err
                error = _err
                
                if ml_lwc then -- in a multi-line command
                    ml_lwc = ml_lwc ..' '.. line
                    if not line:match("[\\]$") then
                        frame = lwcmd(ml_lwc:gsub("\n",' '),frame,stack)
                        ml_lwc = nil
                    end
                elseif line:match("^[#][%u].*[\\]$") then -- multi-line LuaWrap command start
                    ml_lwc = line:match("^#([%u].*)[\\]$")
                elseif line:match("^[#][%u].*[^\\]$") then -- one line LuaWrap commands
                    frame = lwcmd(line:match("^[#]([%u].*)$"),frame,stack)
                elseif line:match('^[#]include%s+["]([[%w_/.]+[.]h)["]') and frame.base then -- #include
                    local incl = line:match('^[#]include%s+["]([[%w_/.]+[.]h)["]')
                    local new_fr = reader(incl,add,vadd,finish);
                    table.insert(stack,new_fr)
                    frame = stack[#stack]
                elseif line:match("^[%w_][%w_%s*]+%s+[%w_%s*]*[(].*;%s*$") then -- C function prototypes an typedefs and other stuff
                    local proto = {
                            args={},argv={},line=line,file=frame.filename,ln=frame.ln
                        }
                    local what
                    
                    if line:match("^typedef ") then
                        local logline = "CB typedef: " .. line
                        log(1,logline)
                        frame.vadd(1,"// " .. logline)
                        
                        line = line:gsub("^typedef%s+","")
                        line = line:gsub("[(]%s*[*]%s*([%w_]+)%s*[)]","%1")
                        proto.typedef = true
                        what = 'typedef'
                    else
                        log(1,"C prototype: " .. line)
                        frame.vadd(1,"// C prototype: " .. line)
                        what = 'prototype'
                    end

                    local i = 0;

                    proto.type, proto.name, proto.arglist =
                        line:match("%s*([%w_]+%s*)[%s*]+([%w_]+)%s*[(](.*)[)]")
                    
                    if not (proto.type and proto.name and proto.arglist ) then
                        EF("cannot fetch %s from line...",what)
                    else
                        if not proto.arglist:M("[.][.][.]$") then
                            
                            if proto.arglist ~= 'void' and proto.arglist ~= ''  then
                                for arg in (proto.arglist..','):gmatch("([^,]+),") do
                                    i = i + 1
                                    local t, name = arg:match("%s*([%w_]*%s*[%w_]*%s*[%w_]+[%s*]+)([%w_]+)%s*")

                                    if (t and name) then
                                        local a = {}
                                        a.type, a.name = t, name;
                                        a.idx = i
                                        proto.args[a.name]=a
                                        table.insert(proto.argv,a)
                                    else
                                        log(0,"Cannot fetch %s from line...",what)
                                    end
                                end
                            end
                            log(3,proto)
                            prototypes[proto.name] = proto;

                        else
                            log(1,"C prototype: has ... ignoring")
                        end
                    end
                end
                
                if debug_filebreak == frame.filename and debug_linebreak == frame.ln then
                    E("Break!")
                end

            end
            if frame.finish then frame.finish() end
            table.remove(stack)
            log(4,"done with file:", frame.filename)
        end
    end
    
    local function usage() 
        print("usage: " .. arg[-1] .." ".. arg[0] .. " [verbosity] <infile> <outfile>")
        os.exit(1)
    end

    if not arg[1] then
        usage()
    end
    
    local fname, add, vadd, finish

    if arg[1]:match('^[%d]+$') then
        verbose = tonumber(arg[1])
        fname = arg[2] or usage()
        if arg[3] then
            add, vadd, finish = output(arg[3],f("%s %s %s %s %s",arg[-1],arg[0],arg[1],arg[2],arg[3]))
        else
            usage()
        end
    else
        fname = arg[1]
        if arg[2] then
            add, vadd, finish = output(arg[2],f("%s %s %s %s",arg[-1],arg[0],arg[1],arg[2]))
        else
            usage()
        end
    end

    local log_fd = {close=function()end}
    
    if (verbose > 0) then 
        local efn = f("%s.log",fname);
        log_fd = io.open(efn,'w') or EF("cannot open file: '%s'",efn)
        dump = function(s) 
            log_fd:write(s .. "\n")
            --io.stderr:write(s .. "\n")
        end
    end
    
    process(fname, add, vadd, finish)
    
    log_fd:close();
    
end 
