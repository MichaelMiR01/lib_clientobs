# basic application design
# App
lappend auto_path .
package require lib_clientobs
itcl::class Distributor {
    inherit ClOClass
    
    constructor {} {
       # set _Toplevel [namespace which $this]
       # wm protocol . WM_DELETE_WINDOW [list [namespace which $this] cleanup]
        namespace eval :: [create_classes $this]
    }
    destructor {
    }
    method cleanup {} {
        notifyall _shutdown
        catch {destroy}        
        exit
    }
}

proc create_classes {root} {
    #
    set classes {A B C D}
    set implements {A {M1 M2} B {M3 M4} C M5 D M6}
    set requires {A {M4 M5} B {M1 M2 M6} C M3 D M4}
    set src ""
    foreach class $classes {
        set imps [dict get $implements $class]
        set reqs [dict get $requires $class]
        set meths {}
        set events {}
        set test "method test {} {\n"
        foreach imp $imps {
            set meth [list "method handle$imp" "{}" "{puts \"implemented $imp in \$this\"}"]
            set meth [join $meth " "]
            lappend meths $meth
            lappend events "bindEvent $imp handle$imp"
        }
        foreach req $reqs {
            set meth [list "method call$req" "{}" "{[namespace which $root] notify $req}"]
            set meth [join $meth " "]
            lappend meths $meth
            append test "call$req\n"
        }
        append test "}\n"
        
        lappend events "bindEvent test test"
        
        set def "itcl::class $class {\ninherit ClOClass\n"
        append def "constructor {} {\n"
        append def [join $events \n]
        append def "\n[namespace which $root] attach \$this\n"
        append def "}\n" 

        append def "destructor {\n}\n" 

        append def [join $meths \n]
        append def "\n"
        append def $test
        append def "}\n"
        append def "set c \[$class #auto\]\n"
        append src "$def\n"
    }
    
    return $src
}

Distributor #auto
^^ Distributor notify test
