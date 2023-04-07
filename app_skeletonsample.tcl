#!/usr/bin/tclsh
#sample app using lib_clientobject ClOClass
lappend auto_path .
package require lib_clientobs
catch {console show}
itcl::class Application {
    inherit ClOClass
    
    constructor {} {
        set _Toplevel [namespace which $this]
    }
    destructor {
        #
    }
}

itcl::class GUI {
    inherit ClOClass
    constructor {} {
        bindEvent test OnTest export
        bindEvent test1 OnTest1 export
        bindEvent updategui OnUpdategui
        bindEvent guigetvalue OnGetValue
        # use export and ::ClOClass::define_unknown to create pseudo-global procs :-)
        # bindEvent test OnTest export
        # ::ClOClass::define_unknown
        # test "this is a test"
    }
    method OnTest {t} {
        puts "test: $this $t"
        return "Test from GUI ok, got $t"
    }
    method OnTest1 {t} {
        puts "test1: $this $t"
        notify test subroutines
        return "Test1 ok"
    }
    method OnUpdategui {t} {
        puts "Updating: $this $t"
        return "Update gui ok, got $t"
    }
    method OnGetValue {c} {
        return [expr $c*7];
    }
}
itcl::class NullModel {
    # this can be used to find all model classes with _searchMemberbyClass NullModel * 0 1
    constructor {} {
        #
    }
}

itcl::class Model {
    inherit ClOClass NullModel
    constructor {} {
        Relaymodel relay
        bindEvent test OnTest export
        delegateEvent relaytest [namespace which relay] instance test 
        #$this attach [namespace which relay]
    }
    method OnTest {t} {
        puts "test: $this $t"
        App notify updategui "$this says $t"
        return "Test from model ok"
    }
}

itcl::class NullGui {
    # this can be used to find all model classes with _searchMemberbyClass NullGui * 0 1
    constructor {} {
        #
    }
}

itcl::class Submodel {
    inherit ClOClass NullModel
    constructor {} {
        bindEvent bubbletest OnBubbleTest
        bindEvent test OnTest
    }
    method OnTest {t} {
        puts "test in submodel: $this $t"
    }
    method OnBubbleTest {t} {
        puts "bubbletest: $this $t"
        
        # now a little sample of how to get some results back
        # results are collected from all returning calls
        # into a list
        
        # call test from toplevel
        puts "Getting Value from Gui"
        set r [notifytop guigetvalue 6]
        # get resultlist
        set results [getResults $r]
        puts "result: $results"
        foreach result $results {
            lassign $result caller call callargs replier reply
            puts "$caller calls $call"
            puts "$replier answers $reply"
            # notice, we still don't need to know the gui's real instance, just it's class
            # we could even filter heuristical on the string gui
            if {[$replier info class] eq "::GUI"} {
                puts "Gui replied $reply"
            }
        }
        return "Bubbletest ok"
    }
}
itcl::class Subgui {
    inherit ClOClass NullGui
    constructor {} {
        bindEvent test OnTest
    }
    method OnTest {t} {
        puts "test in subgui: $this $t"
        return "Subgui ok"
    }

}


itcl::class Relaymodel {
    inherit ClOClass NullModel
    constructor {} {
        # Relaymodel is totally detached from everything
        # and only gets delegatedEvents from model
        # so this works like a kind of filter to underlying observers
        # careful, if we attach this to a regular observer chain
        # bubbling a test signal may result in double call to all observers
        Submodel relayed1
        Submodel relayed2
        Submodel relayed3
        $this attach [namespace which relayed1]
        $this attach [namespace which relayed2]
        $this attach [namespace which relayed3]
        bindEvent test OnTest
    }
    method OnTest {t} {
        puts "test in relaymodel: $this $t"
        notify test "$t from relay"
    }
}

itcl::class externalClass {
    constructor {} {
    }
    method test {t} {
        puts "test: $this $t"
        return "Test from externalClass ok"
    }
}


proc main {} {
    
    # example, that follows a Model-View-Controller paradigm
    # Controller is our Application Class
    # View is the Gui
    # Model is divided into Model and Submodel
    
    # init our controller
    Application App
    
    # we explicitly build and attach the Applications subclasses here
    # for better understandability,
    # but this could as well be done in the App constructor or similar
    
    
    GUI gui
    Subgui subgui1
    Subgui subgui2
    Subgui subgui3
    Model model
    Submodel submodel
    
    externalClass xtc
    
    # Structure looks like this
    # App -> Model -> Submodel
    # App -> Gui
    App attach gui
    gui attach subgui1
    gui attach subgui2
    gui attach subgui3
    
    App attach model
    model attach submodel
    
    # let's see what happens, when we add an external class, that is not ClOClass derived
    App attach xtc
    
    # btw you can even attach commands as observers, if they have proper subcommands
    # but generally this is NOT advised
    # example
    # App attach "::info"
    # App notify commands
    # # this will do the same like info commands
    # and getting the result from this gives a list of commands as seen from Apps namespace...
    #
    # it is even possible to attach Tk widgets
    if {1} {
     package require Tk
     button .b1 -text BUTTON -command {
        puts "BUTTON got invoked"
        ClOClass::_notifytop test1 "Button invoked"
     }
     pack .b1
     # attach and notify
     puts "Calling button from gui"
     gui attach .b1
     gui notify invoke
     gui detach .b1
     
     # or delegate
     puts "Calling button as delegate of buttontest"
     gui delegateEvent buttontest .b1 instance invoke
     ClOClass::_notifytop buttontest
     gui delegateEvent buttontest ""
     puts "ready"
    }
    # App will notify Model and Gui    
    puts "Calling via App notify test"
    App notify test "this is a test"

    # to call from a global context without knowing where your app is use
    # ::ClOClass::_notifytop
    # App has to put its fully qualifed path into _Toplevel, like we do in this samples constructor
    # e.g. after 2000 {::ClOClass::_notifytop test 123}
    
    puts "Calling as proc"
    # ::ClOClass::define_unknown will handle all enevent bound as export as _notifytop events 
    ::ClOClass::define_unknown
    test "Test to unknown proc"
    
    
    # bubble will let the event bubble down till someone uses it
    # so bubble test will be caught on the first level
    puts "bubbling test"
    App bubble test ooo
    # bubble bubbletest is implemented in Submodel, wich is under model 
    puts "bubbling bubbletest"
    App bubble bubbletest ooo
    # btw, bubbletest gave a call to updategui, did you notice?
    
    # backpropagate is like bubble, just the other direction
    # it calls a classes client and notifys upward until someone listens 
    puts "backpropagate model test ooo"
    model backpropagate  test <ooo>
    puts "backpropagate model test1 ooo"
    model backpropagate  test1 <ooo>
    
    # lets test some delegation
    # wich we can introduce at anytime
    puts "Testing delegation in model --> submodel"
    model delegateEvent test submodel
    App notify test 123
    # and revoke
    puts "Removing delegation"
    model delegateEvent test {}
    App notify test 123
    
    puts "Testing relay"
    App notify relaytest "relayed test"
    
    puts "Testing _seachMemberByClass on NullModel"
    set obs [::CommonAppStruct::_searchMemberByClass NullModel * 0 1]
    puts "Found $obs"
    App deliver2 $obs test "I'm a NullModel"
    
    puts "Testing _seachMemberByClass on NullGui"
    set obs [::CommonAppStruct::_searchMemberByClass NullGui * 0 1]
    puts "Found $obs"
    puts [App deliver2 $obs test "I'm a NullGui"]
    
    # Application structure is now dumped
    ClOClass::dump
}

main
