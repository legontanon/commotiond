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
    
    local verbose = 6 -- I'm the verbosity level
    local own_prefix  = arg[0]:match("^(.*/)[%w_.]") or '' -- I hold the own prefix to the script (and its files)
    local module_name

    -- shortcuts
    local E = error -- the error function (will likely change at each line read)
    local f = string.format
    string.f = string.format
    local function EF(...) return E(f(...)) end
    local M = string.match
    string.M = M
    
    function table.top(t) return t[#t] end
    function table.shift(t) return t:remove(1) end
    table.pop = table.remove
    table.push = table.insert
    
    local lqw_cmd = {}; --  //LW: commands execution
    local mode = 'static'

    local function dup_to(t) -- I create tables that will copy to the given table (err if key exists)
        -- used to guarantee uniqueness of names between different tables, or if called as dup_to({}) a single table
        return setmetatable({},{
            __newindex=function(o,k,v)
                local e = t[k];
                if e then
                    EF("duplicate name: '%s' previously used in %s:%d",k, o.filename, o.ln)
                end
                rawset(o,k,v);
                t[k]=v;
            end, 
            __index=rawget,
        })
    end
    
    local types = {}; -- holds all named types (classes, enums, callbacks, functions)
    local prototypes = {} -- I hold the collection of prototypes (by name)
    local classes = dup_to(types) -- I hold the collection of classes (by name)
    local functions = dup_to(types) -- I hold the collection of functions that are not in a class (by name)
    local enums = dup_to(types) -- I hold the enumerations (by name) -- nothing for now
    local callbacks = dup_to(types) -- I hold the callbacks (by name)
    local values = dup_to(types) -- I hold Lua values to be added (by name) 
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
        local s = F("%{proto.type} _ = %{proto.name}(",m)

        for i,a in pairs(m.proto.argv) do
            s = s ..F("%{name}, ",a)
        end

        s = s:sub(1,-3)

        return s .. ");"
    end
    
    
    
    local farg_t2C = { -- function arguments for each Ts
        N=function(a) 
            test = a.name or EF("N-argument[%d] without a name",a.idx);
            if type(a.default) == "string" and a.default ~= "" then
                return F("  %{type} %{name} = optN(L, %{idx}, %{type}, %{default}); /* arg[%{idx}] %{text} */\n",a)
            else
                return F("  %{type} %{name} = checkN(L,%{idx},%{type}); /* arg[%{idx}] %{text} */\n",a)
            end
        end,
        B=function(a) 
            test = a.name or EF("B-argument[%d] without a name",a.idx);
            if a.default and a.default ~= "" then
                return F("  int %{name} = optB(L, %{idx}, (int)%{default}); /* arg[%{idx}] %{text} */\n",a)
            else
                return F("  int %{name} = checkB(L, %{idx}); /* arg[%{idx}] %{text} */\n",a)
            end
        end,
        S=function(a)
            test = a.name or EF("S-argument[%d] without a name",a.idx);
            if a.default and a.default ~= '' then
                return F("  char* %{name} = optS(L, %{idx},%{default}); /* arg[%{idx}] %{text} */\n",a)
            else
                return F("  char* %{name} = checkS(L, %{idx}); /* arg[%{idx}] %{text} */\n",a)
            end
        end,
        L=function(a)
            test = a.name or EF("L-argument[%d] without a name",a.idx);
            test = a.len_name or EF("L-argument[%d] '%s' without len_name",a.idx,a.name)
            test = a.type or "const char*";            
            a.len_type = a.len_type or 'size_t';
            return F('  %{len_type} %{len_name}; %{type} %{name} = checkL(L, %{idx}, &%{len_name}); /* arg[%{idx}] %{text} */\n',a)
        end,
        X=function(a)
            test = a.name or EF("X-argument[%d] without a name",a.idx);
            test = a.type or EF("X-argument[%d] '%s' without a type",a.idx,a.name);
            test = a.value or EF("X-argument[%d] '%s' without a value",a.idx,a.name);
            
            return F("  %{type} %{name} = %{value};  /* arg[%{idx}] %{text} */\n",a)
        end,
        O=function(a) 
            test = a.name or EF("%s-argument[%d] '%s' without a name",a.type,a.idx);
            
           if a.default and a.default ~= '' then
                return F("  %{type}* %{name} = opt%{type}(L, %{idx}, %{default});  /* arg[%{idx}] %{text} */\n",a)
            else
                return F("  %{type}* %{name} = check%{type}(L, %{idx});  /* arg[%{idx}] %{text} */\n",a)
            end
        end,
        F=function(a)
            local cb = callbacks[a.type] or EF("No such Callback '%s'",a.type);
            
            if cb.mode == 'key' then
                test = a.key_name or EF("Keyed Callback '%s' without key_name",a.type)
                
                return F("  %{type} %{name} = check%{type}(L, %{idx}, %{key_name});  /* arg[%{idx}] %{text} */\n",a)
            else
                return F("  %{type} %{name} = check%{type}(L, %{idx});  /* arg[%{idx}] %{text} */\n",a)
            end    
        end,
        K=function(a)
            test =  a.name or EF("K-argument[%d] '%s' without a name",a.idx);
            return F("  %{type} %{name} = checkF(L, %{idx}, %{cname});  /* arg[%{idx}] %{text} */\n",a)
        end,
    }
        
    
    local fret_t2C = { -- function returs for Ts
        N=function(r,m)
            test = r.name or EF("N-return[%d] without a name",r.idx);
            return F("  %{type} %{name}; /* ret[%{idx}] %{text} */\n",r),
                F("  pushN(L,%{name});  /* ret[%{idx}] %{text} */\n",r)
        end,
        S=function(r,m)
            test = r.name or EF("S-return[%d] without a name",r.idx);
            return F("  const char* %{name}; /* ret[%{idx}] %{text} */\n",r),
                F("  pushS(L,%{name});  /* ret[%{idx}] %{text} */\n",r)
        end,
        B=function(r,m)
            test = r.name or EF("B-return[%d] without a name",r.idx);
            return F("  int %{name}; /* ret[%{idx}] %{text} */\n",r),
                F("  pushB(L,%{name});  /* ret[%{idx}] %{text} */\n",r)
        end,
        L=function(r,m)
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
            test = r.name or EF("%s-return[%d] without a name",r.type,r.idx);
            return F("  %{type}* %{name};  /* ret[%{idx}] %{text} */\n",r),
                F("  push%{type}(L, %{name});  /* ret[%{idx}] %{text} */\n",r)
        end,
    }
    
    local function function2C(m) -- given a method I yield C code for it
        test = m.name or E("function without a name");
        test = m.proto or EF("function '%s' without a prototype",m.name);

        local s = F("// calling: %{proto.line}\nstatic int %{codename}(lua_State* L) {\n",m)
        local s2 = ''
            
        for idx,arg in pairs(m.args) do
            if arg.name ~= '_' then
                s2 = s2 .. farg_t2C[arg.t](arg,m)
            end
        end
        
        s2 = s2 .. "  " .. proto2C_call(m) .. "\n"

        for idx,ret in pairs(m.rets) do
            local _d, _s = fret_t2C[ret.t](ret,m)
            
            if ret.name ~= '_' then
                s = s .. _d
            end
            
            s2 = s2 .. _s

        end

        return f("%s%s  return %d;\n}\n",s,s2,#m.rets)
    end
    
    local function accessor2C(a) -- given a method I yield C code for it
        test = a.name or E("function without a name");
        test = a.proto or EF("function '%s' without a prototype",m.name);

        local s = F("static int %{cname}_%{name}(lua_State* L) {\n %{cname}* __o = check%{cname}(L,1);\n",a)

        if a.type == 'RW' then 
            s = s .. ' if (lua_gettop(L)>1) {\n'
            for name,_in in pairs(a.ins) do
                local function SNB(arg) return (" (%{expr}) = check%{t}(L,%{idx});"):F(arg) end
                s = s .. {
                    L=function(arg) return (" (%{expr}) = checkL(L,%{idx},&(%{len_expr}));"):F(arg) end,
                    O=function(arg) return (" (%{expr}) = check%{type}(L,%{idx}));"):F(arg) end,
                    S=SNB,N=SNB,B=SNB
                }[arg.t](_in)
            end
            s = s .. ' } else {\n'
        end
        
        for idx,ret in pairs(m.outs) do
            local function SNB(arg) return (" push%{t}(L,(%{expr}));"):F(arg) end
            s = s .. {
                L=function(arg) return (" pushL(L,(%{expr}),&(%{len_expr}));"):F(arg) end,
                O=function(arg) return (" push%{type}(L,(%{expr})));"):F(arg) end,
                S=SNB,N=SNB,B=SNB
            }[arg.t](ret)        end

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
        local b =  "  lua_settop(L,0);\n  lua_newtable(L);\n"

        for n,c in pairs(classes) do
            s = s .. F("  lual_Reg* %{name}_methods = {\n",c)
            for name, m in pairs(c.methods) do
                s = s .. F('    {"%{name}",%{codename}},\n',m)
            end
            s = s .. "    {NULL,NULL}};\n"
                        
            s = s .. F("  lual_Reg* %{name}_metas = {\n",c)
            for name, m in pairs(c.meta) do
                s = s .. F('    {"%{name}",%{codename}},\n',m)
            end
            s = s .. '    {"__gc",%{name}___gc},\n    {NULL,NULL}};\n'
            
            b = b .. F('  LqwClassRegister(%{name});\n',c)

            for name, v in pairs(c.values) do
                b = b .. F('  push%{t}(L,(%{expr}));\n  lua_setfield(L,-2,"%{name}");\n',v)
            end
        end
        
        s = s .. "  lual_Reg* functions = {\n"
        for name, f in pairs(functions) do
            s = s .. F('    {"%{name}",%{codename}},\n',f)
        end
        s = s .. "    {NULL,NULL}};\n  lual_Reg* f;\n\n"
        b = b .. "  \n  for (f=functions;f->name;f++) {\n" ..
                 '    lua_pushcfunction(L,f->func); lua_setfield(L,1,f->name);\n  }\n\n'
        
        for name, v in pairs(values) do
            b = b .. F('  push%{t}(L,(%{expr}));\n  lua_setfield(L,1,"%{name}");\n',v)
        end
        
        local cb = '  /* Register C Callbacks */\n'
        for tname, cbf in pairs(callbacks) do
            cb = cb .. "   lua_newtable (L);\n"
            for name, ccb in pairs(cbf.c_cbs) do
                cb = cb .. f('     lwcRegCB(L, -1, "%s", %s,"%s");\n',name,ccb.fn_name,ccb.tname);
            end
            cb = cb .. f('    lua_setfield(L,1,"%s");\n',tname);
        end
        
        return s .. b .. cb .. "  return 1;\n}\n"
    end

    local cb_arg_t = {
        B=function(a) return F("  pushB(L,(int)%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        N=function(a) return F("  pushN(L,(luaNumber)%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        O=function(a) return F("  push%{type}(L,%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        F=function(a) return F("  push%{type}(L,%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        S=function(a) return F("  pushS(L,%{name}); /* arg[%{idx}] %{text} */\n",a) end,
        L=function(a) return F("  pushL(L,%{name},(size_t)%{len_name}); /* arg[%{idx}] %{text} */\n",a) end,
        X=function(a) return F("  %{name} = (%{type})%{value}; /* arg[%{idx}] %{text} */\n",a)
        end,
    }
    
    local cb_ret_t = {
        B=function(a)
            return a.name:M('^[%a][%w_]*$') and F("  int %{name};  /* ret[%{idx}] %{text} */\n",a) or "",
                F("    %{name} = toB(L,%{idx}); /* ret[%{idx}] %{text} */\n",a)
        end,
        N=function(a)
            return a.name:M('^[%a][%w_]*$') and F("  %{type} %{name};  /* ret[%{idx}] %{text} */\n",a) or "",
                F("    %{name} = toN(L,%{idx},%{type}); /* ret[%{idx}] %{text} */\n",a)
        end,
        O=function(a)
            return a.name:M('^[%a][%w_]*$') and F("  %{type}* %{name};  /* ret[%{idx}] %{text} */\n",a) or "",
                F("    %{name} = to%{type}(L,%{idx}); /* ret[%{idx}] %{text} */\n",a)
        end,
        S=function(a)
            return a.name:M('^[%a][%w_]*$') and F("  const char* %{name};  /* ret[%{idx}] %{text} */\n",a) or "",
                F("    %{name} = toS(L,%{idx}); /* ret[%{idx}] %{text} */\n",a)
        end,
        L=function(a)
            local len_def, var_def
            
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
        D(c)
        local start = ""
        local call = ""
        local check = 'LQW_MODE %{name} check%{name}(luaState* L, int idx'

        if c.mode == 'closure' then
            start = "static int %{name}__fn_idx;\n"
            check = check .. ") {\n"
            call = call  .. '  lua_replace(L, %{name}__fn_idx );\n'
        else
            call = '  __fn_idx = lua_gettop(L)+1;\n'
                .. '  int __res;\n'
                .. '  const char* __key = cbKey%{name}((void*)(%{key_name}));\n'
              .. '\n  lua_getfield(L, LUA_REGISTRYINDEX, __key );\n'
            
            check = check .. ", void* %{key_name}) {\n"
        end

        check = check .. "  %{proto.name} fn;\n"
        if c.mode == 'closure' then
            check = check .. '  %{name}__fn_idx = idx;\n fn = checkCB(L, idx, call%{name}, NULL, NULL, "%{name}");\n'
        else 
            check = check .. '  fn = checkCB(L, idx, call%{name}, cbKey%{name}, %{key_name}, "%{name}");\n'
        end
        check = check .. "\n return fn;\n}\n";
        
        start = start .. "LQW_MODE %{proto.type} call%{name}(%{proto.arglist}) {\n  luaState* L = lwState();"
                      .. (c.proto.type ~= 'void' and "\n  %{proto.type} _ = %{ret_def};\n" or "")

        for idx, a in ipairs(c.args) do
            local c = cb_arg_t[a.t] or function(a) return "" end
            call = call .. c(a);
        end
                
        call = call .. f("  if((__res = lua_pcall(L, %d, %d, 0})== LUA_OK)){\n",#c.args,#c.rets)
        
        for idx,r in ipairs(c.rets) do
            local c = cb_ret_t[r.t] or EF("return %{name} is of invalid type '%{t}'",r);
            local decls, body = c(r);
            
            start = start .. decls
            call = call .. body
        end
        
        call  = call .. f("    lua_pop(L,%d)\n",#c.rets)
        
        if c.proto.type ~= 'void' then
            call = call .. "    return _;\n"
        else
            call = call .. "    return;\n"
        end
                
        call = call .. '  } else lqwPcallErr(__key,__res);\n\n'

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
        --log(5,'<splitList',list,v2s(t_pars),v2s(rets))
        tbl = tbl or {}
        local i = 0
        
        for ty, par in list:gmatch("(%u[%w]*)[{]%s*([^}]*)%s*[}]") do
            log(0,"%s = %s",ty,par)
            i = i + 1
            local r = {idx = i, text = f("%s{%s}",ty,par)}
            local t_par = t_pars[ty]
            
            if t_par then
                r.t = ty
            else
                local t = (types[ty] or EF("no such type: '%s'",ty)).t
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
        local c = {filename=frame.filename,ln=frame.ln,
            methods=dup_to({}), meta = dup_to({}), values=dup_to({})
        }
        local p = params:split("%s"," ","+");
        
        c.name = p[1]:match("^([%w_]+)$") or E("Class must have a name [A-Z][A-Za-z0-9_]+")
        c.type = p[2]:match("^([%w_]+)$") or E("Class must have a type [A-Za-z0-9]+")
        c.t = 'O';
        classes[c.name] = c

        return c
    end
    
    function lqw_cmd.Class(params,val,frame)
        log(2,'Class:',params,val)
        local c = class(params,val,frame)
        log(3,'Class:',c)
        local s = class2C(c)
        frame.add(s);
    end
    
    function lqw_cmd.OwnClass(params,val,frame)
        log(2,'OwnClass:',params,val)
        local c = class(params,val,frame)
        c.own = true
        log(3,'OwnClass:',c)
        local s = class2C(c)
        frame.add(s);
    end

    function lqw_cmd.Alias(params,val,frame)
        local c,n,tc,tn = params:M("^%s*(%u[%w_]+)[.]?([%w_]+)%s+(%u[%w_]+)[.]([%w_]+)%s*$")
        test = c and c and tc and tn or E("Malformed Alias")
        local (d = tc~='' and functions) or (classes[tc]or E("no such class '%s'",tc))[tn:M('^__') and 'meta' or 'methods']
        local (s = c ~='' and functions) or (classes[c] or E("no such class '%s'",c ))[ n:M('^__') and 'meta' or 'methods']
        d[n] = s[tn]
    end
   

    local acc_NSB = {"expr"}
    local acc_t = {
        N=acc_NSBO,   
        S=acc_NSBO,
        B=acc_NSBO,
        O=acc_NSBO,
        L={"expr,len_expr"}
    }
    
    function lqw_cmd.Accessor(params,val,frame)
-- #Accessor Str.__tostring RO: L{$data;$_len}
-- #Accessor Uint32.value RW: N{$data} @ N{$data}
        
        local a = { }
        a.cname, a.name, a.type = params:M("^%s*(%u[%w]+)[.]([%w_]+)%s+(R[WO])%s*$")
        local c = classes[cname] or E("No such class '%s'",cname)
        local ins, outs = val:M("^%s*([^@]*)%s*@%s*(.*)%s*$")
        a.ins = splitList(ins,acc_t)
        a.outs = splitList(outs,acc_t)
        
        
        c[a.name:M("^__") and 'meta' or 'methods'][a.name] = a
        
        local s = accessor2C(a)
    end
    
    function lqw_cmd.OwnFunction(params,val,frame)
        log(5,'OwnFunction:',params,val)
        
        
        local cname, name = params:M("^%s*(%u[%w]+)[.]([%w_]+)%s*$")

        if not cname then
            name = params:M("^%s*([%w_]+)%s*$") or EF("Didn't found a valid name")
            functions[name] = {
                fullname=name, name=name, codename=f("%s_%s",module_name,name),
                own=true, filename=frame.filename, ln=frame.ln
            }
        else
            local c = class[cname] or EF("No such class %s",cname)
            local cont = name:M("^__") and c.meta or c.methods
            cont[name] = {
                fullname=f("%s.%s",cname,name),cname=cname, name=name, codename=f("%s_%s",cname,name),
                own=true, filename=frame.filename, ln=frame.ln
            }
        end
    end

    function lqw_cmd.OwnCallBack(params,val,frame)
        log(5,'OwnCallBack:',params,val)
        local m={own=true, filename=frame.filename, ln=frame.ln, is_cb=true, t='F', c_cbs={}}
        local m.name, m.key_name = params:match("^%s*(%u[%g]+)[{]?([%w_]*)[}]?%s*$")
            
        test = m.name or EF("CallBack %s has no good name",params);
            
        mode = (m.key_name and m.key_name ~= '' and 'key') or 'closure'
            
        callbacks[name] = m
    end

    local finished= false
    
    function lqw_cmd.Finish(params,val,frame)
        log(5,'Finish:',params,val)
        local fname = params:M("^%s*([%g_]+)%s*$") or E("Not a valid registration function name");
        local s = registration2C(fname);
        frame.add(s);
        finished = true
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
    }
    
    local ret_t_pars = { -- options for return argument parts
        N={"name","default"},
        S={"name","default"},
        B={"name","default"},
        L={"name","len_name","bufsize","len_type","len_def","default"},
        O={"name","ref","default"},
    }        
    
    function lqw_cmd.Function(params,val,frame,m)
        
        log(2,'Function:',params,val)
        local m = m or {
            filename=frame.filename, ln=frame.ln,
            rets=dup_to({}), args=dup_to({}), names=dup_to({}),
            params=params, expr=val
        }
        
        local pars = params:split("%s"," ",'+');
        m.fullname = shift(pars) or E("Function/Callback must have name parameter")
        m.fn_name = shift(pars) or E("Function/Callback must have fn_name parameter")        
        m.proto = prototypes[m.fn_name] or E("no prototype found for: " .. v2s(m.fn_name) )
        
        local collection
        if m.is_cb then
            test = m.proto.typedef or EF("Callback prototype must be a callback prototype");
            m.ret_def = shift(pars)
            
            m.name, m.key_name = m.fullname:match("%s*(%u[%g]+)[{]?([%w_]*)[}]?%s*")
            
            test = m.name or EF("CallBack %s has no good name",m.fullname);
            
            m.mode = m.key_name ~= '' and 'key' or 'closure'
            
            if m.proto.type ~= 'void' and not m.ret_def then
                EF("Callback '%s' has non void return type but was given no return default.",m.name)    
            end
            
            collection = callbacks
        else
            if m.fullname:match("%s*(%u[%w]+)[.]([%w_]+)%s*") then
                m.cname, m.name = m.fullname:match("(%u[%w]+)[.]([%w_]+)")
                m.codename = m.cname .. "_" .. m.name
                m.is_meta = m.name:M("^__") and true or false
                local class = classes[m.cname] or E("no such class: " .. v2s(m.cname))

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
            
        local i = 0
        local ret, args = val:match("%s*([^@]*)%s*[@]?([^@]*)%s*")
        
        test = (ret and args) or E("malformed method:" .. v2s(m.fn_name) )
        
        splitList(ret,ret_t_pars,m.rets)
        splitList(args,arg_t_opts,m.args)
        
        for k,a in pairs(m.args) do
            if not a.type and a.t ~= 'K' and a.t ~= 'F'  then
                if m.proto.args[a.name] then
                    a.type = m.proto.args[a.name].type
                else
                    EF("In function %s untyped argument: '%s' is not in prototype",m.fullname,a.name)
                end
            end
        end
                
        collection[m.name] = m
        frame.add((m.is_cb and cb2C or function2C)(m))
        log(3,"Function",m)
    end

    function lqw_cmd.CallBack(params,val,frame)
        
        local cb = lqw_cmd.Function(params,val,frame,{
            is_cb=true,filename=frame.filename, ln=frame.ln,
            rets=dup_to({}), args=dup_to({}), names=dup_to({}), params=params, expr=val, t='F', c_cbs=dup_to({})
        })
    end
    
    
    function lqw_cmd.Value(params,val,frame)
        local v = {}
        v.cname, v.name, v.t, v.expr = params:M("^%s*(%u?[%w]*)[.]?([%w_]+)%s+(%u[%w_]*)%s+(.*)$")
        local collection
    
        test = v.t:M("^[NSB]$") or ( classes[v.t] or EF("No such type for Value: '%s'",v.t) )            
            
        if (v.cname) then
            v.fullname = v.cname .. '.' .. v.name
            
            (classes[v.cname] or EF("no such class: '%s'",v.cname)).values[v.name] = v 
        else 
            v.fullname = v.name
            values[v.name] = v;
        end

    end
    

    function lqw_cmd.CCallBack(params,val,frame)
        log(5,'CCallBack:',params,val)
        local m={own=true, filename=frame.filename, ln=frame.ln, is_cb=true, t='F',c}
        local m.tname, m.name, m.fn_name = params:match("^%s*(%u[%w]+)%s+([%w_]+)%s+([%w+]+)%s*$")
        test = m.tname or E("Invalid CallBack name!")
        test = m.name or E("Invalid name!")
        test = m.fn_name or E("Invalid fn_name!")
        
        m.proto = prototypes[m.fn_name] or EF("no prototype found for: '%s'",m.fn_name) 

        test = m.proto.typedef and EF("Prototype '%s' must be that of a function.",m.fn_name);
                    
        mode = (m.key_name and m.key_name ~= '' and 'key') or 'closure'
            
        local c = callbacks[m.tname] or EF("no such CallBack '%s'",m.tname)
        c.c_cbs[m.name] = m;
        
    end
    
    local function lwcmd(lwc,frame,stack)
        local cmd,params,val = lwc:match("(%u[%w]+)%s*([^:]*)[:]?%s*(.*)%s*")
        log(1,"LWD:", lwc,cmd,params,val)
        frame.add(f("// %s:%d #%s\n",frame.filename,frame.ln, lwc))
        if cmd != 'Module' and not module_name then E("no module defined") end
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
                elseif line:match("^%a[%w_%s*]+%s+[%w_%s*]*[(].*;%s*$") then -- C function prototypes an typedefs
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
                    D(proto)
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
                                        EF("Cannot fetch %s from line...",what)
                                    end
                                end
                                log(3,proto)
                                prototypes[proto.name] = proto;
                            end
                        else
                            log(1,"C prototype: has ... ignoring")
                        end
                    end
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
        dump = function(s) log_fd:write(s .. "\n"); io.stderr:write(s .. "\n"); end
    end
    
    process(fname, add, vadd, finish)
    
    log_fd:close();
    
end 
