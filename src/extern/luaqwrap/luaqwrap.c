/**
 *       @file  luaqwrap.c
 *      @brief  An API wrapper for the Lua programming Language
 *
 *     @author  Luis E. Garcia Ontanon <luis@ontanon.org>
 *
 *     Created  01/07/2014
 *    Revision  $Id:  $
 *    Compiler  gcc/g++
 *   Copyright  Copyright (c) 2013, Luis E. Garcia Ontanon <luis@ontanon.org>
 *
 *   See LICENSE file for for Copyright notice
 */


#ifndef __LuaQWrap
/* this C include is most likely be dumped into a generated wrapper than compiled
   standalone. The include is unnecessary furthermore the file is likely not
   even to exists at the given location so its fencing.
   */
#include "luaqwrap.h"
#endif

#ifdef LQW_NEED_OPTB
LQW_MODE int luaL_optboolean(lua_State* L, int n, int def) {
    int val = FALSE;

    if ( lua_isboolean(L,n) ) {
        val = lua_toboolean(L,n);
    } else if ( lua_isnil(L,n) || lua_gettop(L) < n ){
        val = def;
    } else {
        luaL_argerror(L,n,"must be a boolean");
    }

    return val;
}
#endif


LQW_MODE lua_State* gL = NULL;
LQW_MODE lua_CFunction lqw_Fmt = NULL;



LQW_MODE int lqwCleanup(void) {
    /* cleanup lua */
    lua_close(gL);
    gL = NULL;
    return 0;
}

LQW_MODE int lqwNop(lua_State* L) { return 0; }

LQW_MODE lua_State* lqwState(void) { return gL; }

LQW_MODE const char* lqwVFmt(const char *f, va_list a) { 
    static char b[ROTBUF_NBUFS][ROTBUF_MAXLEN];
    static int i = 0;
    char* s = b[(i++)%ROTBUF_NBUFS];
    vsnprintf( s , ROTBUF_MAXLEN, f, a);
    return s;
}

LQW_MODE const char* lqwFmt(const char *f, ...)  {
    const char* r;
    va_list a;
    va_start(a, f);
    r = lqwVFmt(f,a);
    va_end(a);
    return r;
}

LQW_MODE const char* lqwV2S(VS* vs, int v, const char* def) {
    for (;vs->s;vs++)
        if (vs->v == v)
            return vs->s;
    return def;
}

LQW_MODE int lqwS2V(VS* vs, const char* s, int def) {
    for (;vs->s;vs++)
        if ( vs->s == s || strcmp(vs->s,s)==0 )
            return vs->v;
    return def;
}

LQW_MODE const char* lua_shiftstring(lua_State* L, int i) {
    const char* p = luaL_checkstring(L, i);

    if (p) {
        lua_remove(L,i);
        return p;
    } else {
        return NULL;
    }
}

LQW_MODE void lqw_getCb(lua_State* L, const char* cbk) {
    getReg(cbk);
}

LQW_MODE void lqw_setCb(lua_State* L, int idx, const char* cbk) {
    checkF(L,-1,NULL);
    setReg(cbk);
}

LQW_MODE void lwqCheckCbsTbl(lua_State* L, int idx, lqwCbKey* cbKeys[]) {
    checkT(L,idx);
    for(lua_pushnil(L);lua_next(L,idx) && *cbKeys; lua_pop(L,1)) checkF(L,-1,NULL);
    lua_pop(L,1);
}

LQW_MODE void lwqSetCbsTbl(lua_State* L, int idx, lqwCbKey* cbKeys[], void* self) {
    int i = 0;
    
    checkT(L,idx);
    
    lua_pushnil(L);
    while(lua_next(L,idx) && cbKeys[i])
        lqw_setCb(L,-1,(*cbKeys[i++])(self));
    lua_pop(L,1);
}

/* following is based on the luaL_setfuncs() from Lua 5.2, so we can use it in pre-5.2 */
LQW_MODE void lqwSetFuncs(lua_State *L, const luaL_Reg *l, int nup) {
  luaL_checkstack(L, nup, "too many upvalues");
  for (; l->name != NULL; l++) {  /* fill the table with given functions */
    int i;
    for (i = 0; i < nup; i++)  /* copy upvalues to the top */
      lua_pushvalue(L, -nup);
    lua_pushcclosure(L, l->func, nup);  /* closure with those upvalues */
    lua_setfield(L, -(nup + 2), l->name);
  }
  lua_pop(L, nup);  /* remove upvalues */
}

LQW_MODE int lqwClassCreate(lua_State* L, const char* name, luaL_Reg* methods, luaL_Reg* meta) {
    /* create new class method table and 'register' the class methods into it */ 
    lua_newtable (L);
    lqwSetFuncs (L, methods, 0);
    /* add a method-table field named '__typeof' = the class name, this is used by the typeof() Lua func */ 
    pushS(L, name);
    lua_setfield(L, -2, "__typeof");
    /* create a new metatable and register metamethods into it */ 
    luaL_newmetatable (L, name);
    lqwSetFuncs (L, meta, 0);
    /* push a copy of the class methods table, and set it to be the metatable's __index field */
    lua_pushvalue (L, -2);
    lua_setfield (L, -2, "__index");
    /* push a copy of the class methods table, and set it to be the 
       metatable's __metatable field, to hide metatable */ 
    lua_pushvalue (L, -2);
    lua_setfield (L, -2, "__metatable");
    /* pop the metatable */
    lua_pop (L,1);
    return 1;
}


LQW_MODE void lqwInit(lua_Alloc alloc,void* alloc_data) {
    gL = lua_newstate(alloc, alloc_data);
}

int lqwError(lua_State* L, const char* error) {
    luaL_error(L, error );
    return 0;
}

int lqwErrorFmt(lua_State* L, const char* fmt, ...) {
    va_list a;
    
    va_start(a, fmt);
    luaL_error(L, lqwVFmt(fmt,a) );
    va_end(a);
    return 0;
}

int lqwArgError(lua_State* L, int idx, const char* error) {
    luaL_argerror(L,idx,error);
    return 0;
}

int lqwArgErrorFmt(lua_State* L, int idx, const char* fmt, ...) {
    va_list a;
    
    va_start(a, fmt);
    luaL_argerror(L,idx, lqwVFmt(fmt,a) );
    va_end(a);
    return 0;
}




void LqwPcallErr(const char* name, int ret, void (*errh_cb)(const char* name,const char* what,const char* where) ) {
    const char* what;
    const char* where;
    // 2do Error Handler (why!)

    what = toS(gL,-1);
    
    luaL_where(gL, 0);
    where = toS(gL,-1);
    lua_pop(gL,2);
    
    errh_cb(name,what,where);
}


void* checkCB(lua_State* L, int idx, void* lua_cb, lqwCbKey cbKey_cb, void* key_ptr, char* tname) {
    int lt = lua_type(L, idx);
    
    if (lt == LUA_TFUNCTION) {
        // it's lua, save the CB under the key and return the lua callback
        // 2do Error Handler

        if (cbKey_cb) { 
            lqw_setCb(L, idx, cbKey_cb(key_ptr));
        }
        return lua_cb;
    } else if (lt == LUA_TLIGHTUSERDATA) {
        // it's a pointer to a function try to fetch its type and check it against the given one
        void* fn = lua_touserdata(L,idx);
        const char* t;
        
        lua_getfield(L, LUA_REGISTRYINDEX,fn);
        
        if (lua_type(L,-1) != LUA_TSTRING) {
            lqwArgError(L,idx,"function pointer given is not a registered callback");
            return NULL;
        }
        
        t = toS(L,-1);
        lua_pop(L,1); // keep the stack balanced
        
        if (strcmp(t,tname) != 0) {
            lqwArgError(L,idx,"pointer is not a valid callback for this type");
            return NULL;
        }
        
        return fn;
    } else {
        lqwArgError(L,idx,"argument is neither a function nor a function pointer");
        return NULL;
    }
}


LQW_MODE void regCB(lua_State* L, int idx, const char* name, void* cb_ptr, const char* tname) {
    /* we register callbacks by keeping its type_name saved under the callback's pointer as key,
       that way we can check that we are passed a pointer to a right kind of function. */
    pushS(L,tname);
    lua_setfield(L, LUA_REGISTRYINDEX,cb_ptr);
    /* we push the function and then we save it in the given table */
    lua_pushlightuserdata(L,cb_ptr);
    lua_setfield(L, idx, name);
}


LQW_MODE void pushTS(lua_State* L, const char* t[], size_t n) {
    int i;
    lua_newtable(L);
    for(i = 0; (n==0 || i<n) && t[i];) {
        pushS(L,t[i]);
        lua_rawseti (L,-2,++i);
    }
}

LQW_MODE const char** checkTS(lua_State* L, int idx, const char** t, size_t n) {
    unsigned i = 0;
    
    if (lua_type(L, idx) == LUA_TTABLE) {
        for (lua_pushnil(L); i<n && t[i] && lua_next(L,idx); lua_pop(L,1), i++) {
            if (lua_type(L,-1) != LUA_TSTRING) {
                lqwArgError(L,idx,"a table element is not a string");
                return t;
            }
            t[i] = toS(L,-1);
        }
        lua_pop(L,1);
    } else {
        lqwArgError(L,idx,"not a table");
    }
    
    return t;
}

#define MAX_TABLE_ITEMS 1024

LQW_MODE lwcTblIt** ckeckT(lua_State* L, int idx, size_t* np) {
    static lwcTblIt* a[MAX_TABLE_ITEMS];
    int i = 0;
    
    memset(a, 0, sizeof(lwcTblIt));

    
    if (lua_type(L, idx) != LUA_TTABLE) {
        lqwArgError(L,idx,"not a table");
        return NULL;
    }
    
    for (lua_pushnil(L); lua_next(L,idx); lua_pop(L,1)) {
        lwcTblIt* it = a[i++];
        if (i >= MAX_TABLE_ITEMS) break;
        
        switch((it->lua_type = lua_type(L,-1))) {
            case LUA_TNIL:
                break;
            case LUA_TBOOLEAN:
                it->data.boolean = toB(L,-1);
                break;
            case LUA_TUSERDATA:
            case LUA_TLIGHTUSERDATA:
                it->data.ptr = lua_touserdata(L,-1);
                break;
            case LUA_TNUMBER:
                it->data.number = toN(L,-1,lua_Number);
                break;
            case LUA_TSTRING:
                it->data.string.s = checkL(L,-1,&(it->data.string.slen));
            default:
                i--;
                break;
        }
    }
    
    lua_pop(L,1);
    
    np && (*np=i);
    a[i]->lua_type = -1;

    return a;
}
