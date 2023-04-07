package provide lib_clientobs 1.11
package require Itcl

# logg levels
# -1    User level,     usually pushed to surface 
# 0     UserApp level,  usually suppressed
# 1     App level,      usually suppressed
# 2     
# 3
# 4
# 5     Sys level Err,  usually suppressed
# 6     Sys level Warn, usually suppressed
# ...
# 10    Sys level Note, usually suppressed
# ...
# 1000 ...should never be reached :-)

itcl::class CommonAppStruct {
    # this class is a super class for all classes
    # making for an applicationwide structure of obejcts
    # so it is possible to call a known method of a known class
    # of an unknown instance :-)
    # usefull if we have some singeltons for applicationwide use (config, diskio etc.)
    # wich we don't want to put into global vars or something
    # both caller and called class have to inherit from CommonAppStruct
    
    public common __members
    public common _Debug 0
    
    constructor {} {
        if {![info exists _Debug]} {set _Debug 0}
        lappend __members [namespace which $this]
    }
    destructor {
        set idx [lsearch  $__members [namespace which $this]]
        if { $idx < 0 } {
            return ""
        }
        set  __members [lreplace $__members $idx $idx]
    }
    if {[string index [package version Itcl] 0]<"4"} {
	puts "Itcl: [package version Itcl] ... Patching tcloo destroy"
        method destroy {} {
            itcl::delete object $this
        }
    }
    method logg {txt {lv 0}} {
        if {$_Debug>$lv} {
            set t "global context"
            catch {set t $this}
            puts "$t: $txt"
        }
    }
    proc dump {} {
        puts "Members:"
        set _obsStruct [lsort $__members]
        foreach obs $_obsStruct {
            set cla "No Classinfo"
            catch {
                set cla [$obs info class] 
            }
            puts "$cla $obs"
        }
    }
    proc _searchMemberByClass {classname {filter *} {exact 0} {all 0}} {
        # searches for instances of classname
        # exact >0 returns only instances of exactly that class
        # exact =0 also returns derived/inherited classes
        # all>0 returns a list of all matches
        # filter can filter on the instance name *gui* for example
        set o -1
        if {$filter!="*"} {
            set l0 [lsearch -inline -glob -nocase -all $__members $filter]
        } else {
            set l0 $__members
        }
        set result {}
        foreach o $l0 {
            set isacla 0
            catch {set isacla [$o isa $classname]} e
            
            if {$isacla} {
                set classname2 [$o info class]
                set classname2 [string range $classname2 2 end]
                if {($classname==$classname2)||($exact==0)} {
                    if {$all==0} {
                        return $o
                    } else {
                        lappend result $o
                    }
                }
                #break
            } else {
                set o -1
            }
        }
        if {$all>0} {
            return $result
        } else {
            return -1
        }
    }
    method _searchMemberByVar {varname} {
        # searches for a class-variable varname
        # and returns it's value if its a class instance
        set isacla 0
        catch {
            set o [$this cget -$varname]
            set isacla [$o info class]
        } e
            
        if {$isacla!=0} {
            return $o
        }
        return -1
    }
    proc _factory {classname args} {
        # works as a factory pattern and hands out a new instance of $classname, calling any method given
        set classname "::$classname"
        if {[itcl::find classes $classname]!=""} {
            if {[catch {set o [$classname #auto]} e]} {
                error "Instance of $classname failed: $e"
                return -1
            } else {
                if {[llength $args]==0} {
                } else {
                    catch {set r [$o {*}$args]}
                }
                return [namespace which $o]
            }
        }
        return -1
    }
    proc ^^ {classname args} {
        # this is the calling method
        # searches an instance of classname or creates one
        # and hands over command & args to it
        # usage: ^^ classname command args
        set o [^ $classname]
        return [$o {*}$args]
    }
    proc ^ {classname} {
        # this is the singelton pattern
        # it searches of any instance of class $classname and returns it
        # otherwise an instance is created
        # works like a singelton pattern if needed
        set o [_searchMemberByClass $classname]
        if {$o==-1} {
            return [_factory $classname]
        } else {
            return [namespace which $o]
        }
    }
    # these methods are shortcuts for $obj cget -varname
    method --> {varname args} {
        # this is the calling method
        # searches for the given instance in $varname
        # and hands the command (arg(0)) over to that instance
        # usage: --> classname command args
        set o [_searchMemberByVar $varname]
        if {$o==-1} {
            error "Class variable not found $varname"
            return -1
        } else {
            return [$o {*}$args]
        }
    }
    method -> {varname} {
        # returns an instance of CLO in variable $varname
        return [_searchMemberByVar $varname]
    }
}
proc _lappendu {_list el} {#appendunique
    upvar 1 $_list list
    if {$el ni $list} {lappend list $el}
}

proc ^^ {classname args} {
    # this works essentially like a singelton pattern, if used properly
    if {[catch {set r [::CommonAppStruct::^^ $classname {*}$args]} e]} {
        error $e
        return -1
    }
    return $r
}

itcl::class ClOClass {
    # ClientObserver Pattern to build Messaging system
    # Two classes inherit from ClOClass
    # ClassA attach ClassB --> make B Observer of A
    # ClassB bindEvent EventnameE MethodM --> bind MethodM to EventnameE
    # ClassA notify Eventname args 
    # --> notifies B (and all other observers) and calls MethodM (if bound...)
    # 
    # to unbind Method from Event: bindEvent Eventname ""
    # 
    inherit CommonAppStruct
    common _uid 0
    common _mid 0
    public variable UID
    # Structure
    public variable _Observers {}
    public variable _Delegates 
    public common _Toplevel
    # operativ 
    public variable _Methods
    public variable _Caller
    public common _Callstack {}
    public variable _dsplock 0
    public common _dspOK 
    public common _opResult
    public variable _shutdowninprogress 0
    public variable garbagecollect 0
    # Debug 
    public common _ObsStruct {}
    public common _EvStruct {}
    public common _EvDelegates {}
    public common _EvExport {}
    public common _NoteWays
    public common _EvOrphans {}
    public common unknown_original ""
    public common _SysEvents {}
    constructor {} {
        set UID $_uid
        incr _uid
        bindEvent _testping _OnTestPing system
        bindEvent _shutdown _OnShutdown system
        bindEvent _idle _OnIdle system
    }
    destructor {
        #
    }
    method _OnTestPing {} {
        set r ""
        append r "[namespace which $this] listening"
        puts $r
    }
    method _OnShutdown {} {
        # implent here for receiving shutdownevents
        if {$_shutdowninprogress==0} {
            incr _shutdowninprogress
            catch {
                OnShutdown
            }
        }
    }
    method _OnIdle {} {
        # placeholder
        # one can build an idle routine and place it into an after idle loop
        OnIdle
    }
    method OnShutdown {} {
        puts "...shutting down $this"
    }
    method OnIdle {} {
        if {$garbagecollect>0} {
            _garbagecollect_obs
        }
    }        
    method _createMID {} {
        set MID $_mid
        incr _mid
        return $MID 
    }
    method attach {obs} {
        _garbagecollect_obs
        set o [namespace which $obs]
        set t [namespace which $this]
        logg "$this attaching $obs ($o)" 10
        set el [list $t $o]
        if {$el ni $_ObsStruct} {lappend _ObsStruct $el}
        if {[lsearch -exact $_Observers $o]<0} {
            lappend _Observers $o
        }
    }
    method attachList {obs} {
        foreach o $obs {
            attach $o
        }
    }
    method detach {obs} {
        set obs2 [namespace which $obs]
        set t [namespace which $this]
        if {$obs2!=""} {
            set obs $obs2
        }
        set idx [lsearch  $_Observers $obs]
        set idx2 [lsearch  $_ObsStruct [list $t $obs]]
        
        logg "idx to detach $obs: $idx ($idx2)" 10
        if { $idx >= 0 } {
            set _Observers [lreplace $_Observers $idx $idx]
        }
        if { $idx2 >= 0 } {
            set _ObsStruct [lreplace $_ObsStruct $idx2 $idx2]
        }
        
        if {[lsearch -index 1 $_ObsStruct $obs]>-1} {
            logg "Error: detaching $obs failed or double instance!" -1
            return
        }
        # no instances left...
        # kill also from evstruct and evexport
        set ev [lsearch -index 0 $_EvStruct $obs]
        while {$ev>0} {
            set _EvStruct [lreplace $_EvStruct $ev $ev]
            set ev [lsearch -index 0 $_EvStruct $obs]
        }
        set ev [lsearch $_EvExport $obs]
        while {$ev>0} {
            set _EvExport [lreplace $_EvExport $ev $ev -]
            set ev [lsearch $_EvExport $obs]
        }
    }
    proc searchObj {{filter *} {recurse 0}} {
        set l [itcl::find objects * -isa ClOClass]
        if {$filter!="*"} {
            set l [lsearch -inline -glob -nocase -all $l $filter]
        }
        return [lsort -unique $l]
        
    }
    method searchChildren {{filter *} {recurse 0}} {
        set ns [namespace qualifiers $this]
        set tl [namespace tail $this]
        set cl [$this info class]
        set l [itcl::find objects ${cl}::* -isa ClOClass]
        if {$recurse>0} {
            incr recurse
            set lo $l
            foreach child $lo {
                set l2 [$child searchChildren $filter $recurse]
                if {[llength $l2]>0} {
                    append l " " $l2
                }
            }
            incr recurse -1
        }
        if {$recurse<2} {
            if {$filter!="*"} {
                set l [lsearch -inline -glob -nocase -all $l $filter]
            }
        }
        return [lsort -unique $l]
    }
    method searchObs {{filter *} {recurse 0}} {
        set l $_Observers
        if {$filter!="*"} {
            set l [lsearch -inline -glob -nocase -all $l $filter]
        }
        if {$recurse>0} {
            set lo $l
            foreach obs $lo {
                set l2 [$obs searchObs $filter $recurse]
                if {[llength $l2]>0} {
                    append l " " $l2
                }
            }
        }
        return [lsort -unique $l]
    }
    method listClients {o} {
        set o [namespace which $o]
        if {$o==""} {
            return {}
        }
        set ostruct [$o cget -_ObsStruct]
        set r {}
        foreach elem $ostruct {
            lassign $elem cl obs
            if {$obs eq $o} {
                lappend r $cl
            }
        }
        return $r
    }

    method bindEvent {ev method {export 0}} {
        _garbagecollect_obs
        set dup [checkDuplicateEvent $ev]
        if {$dup!=""} {
            logg "Warning: Double Event $this binding $ev $method already implemented" 6 
        }
        lappend _EvStruct [list $this $ev $method]
        if {$export=="export"} {
            lappend _EvExport $ev
            set _EvExport [lsort -unique $_EvExport]
        }
        if {$export=="system"} {
            lappend _SysEvents $ev
            set _SysEvents [lsort -unique $_SysEvents]
        }
        
        set _Methods($ev) $method
    }
    method delegateEvent {ev obj {type instance} {alias ""}} {# set type to variable to resolve at runtime
        _garbagecollect_obs
        if {$obj==""} {
            unset -nocomplain  _Delegates($ev)
            #lappend _EvDelegates [list $ev $this $o $alias]
            set dev [lsearch -all -index 0 $_EvDelegates $ev]
            foreach dg $dev {
                set ddg [lindex $_EvDelegates $dg] 
                set th [lindex $ddg 1]
                if {$th eq $this} {
                    set _EvDelegates [lreplace $_EvDelegates $dg $dg]
                }
            }
        } else {
            if {$type=="variable"} {
                set o $obj
            } else {
                set o [namespace which $obj]
            }
            set _Delegates($ev) [list $o $type $alias]
            if {$alias eq ""} {
                set alias $ev
            }
            lappend _EvDelegates [list $ev $this $o $alias]
        }
    }
    proc _boundEvent {ev} {
        if {[lsearch $_EvExport $ev]>-1} {
            return 1
        }
        return -1
    }
    proc checkDuplicateEvent {filter} {
        # checks if event already exists
        # filter=ev for single event
        set evlist {}
        set multiples {}
        # check all multiples
        foreach inst $_EvStruct {
            lassign $inst obj event method
            if {[lsearch $_SysEvents $event]>-1} {
                continue
            }
            if {[lsearch [dict keys $evlist] $event ]>-1} {
                dict lappend evlist $event $inst
            } else {
                dict lappend evlist $event $inst
            }
        }
        if {[lsearch [dict keys $evlist] $filter ]>-1} {
            dict set multiples $filter [dict get $evlist $filter]
        } 
        if {[llength $multiples]>0} {
            catch {$::ClOClass::_Toplevel configure -garbagecollect 1}
        }
        return $multiples
    }
    proc checkDuplicateEvents {{filter ""}} {
        # checks if event already exists
        # filter=ev for single event
        set evlist {}
        set multiples {}
        # check all multiples
        foreach inst $_EvStruct {
            lassign $inst obj event method
            if {[lsearch $_SysEvents $event]>-1} {
                continue
            }
            if {[lsearch [dict keys $evlist] $event ]>-1} {
                if {$filter==""||$filter==$event} {
                    dict lappend evlist $event $inst
                    dict set multiples $event [dict get $evlist $event]
                }
            } else {
                 dict lappend evlist $event $inst
            }
        }
        if {[llength $multiples]>0} {
            catch {$::ClOClass::_Toplevel configure -garbagecollect 1}
        }
        
        return $multiples
    }
    method notify {args} {
        set MID [_createMID]
        set _Caller [namespace which $this]
        lappend _Callstack [list $_Caller $MID]
        #unset -nocomplain _opResult
        set ok [_notify {*}$args]
        set _Caller ""
        set _Callstack [lrange $_Callstack 0 end-1]
        if {$ok==0} {
            logg "Not dispatched $this $args" 6
            lappend _EvOrphans [list --> $this $args]
        }
        return $MID
    }
    method bubble {args} {
        # notify obs level up until someone listens, than break and return
        # will notify all obs in the last level! 
        set MID [_createMID]
        set _Caller [namespace which $this]
        lappend _Callstack [list $_Caller $MID]
        #unset -nocomplain _opResult
        set ok [_bubble {*}$args]
        set _Caller ""
        set _Callstack [lrange $_Callstack 0 end-1]
        if {$ok==0} {
            logg "Not bubbled $this $args" 6
            lappend _EvOrphans [list ooo $this $args]
        }
        return $MID
    }
    method backpropagate {args} {
        # notify clients level up until someone listens, than break and return
        # will notify all obs in the last level! 
        set MID [_createMID]
        set _Caller [namespace which $this]
        lappend _Callstack [list $_Caller $MID]
        #unset -nocomplain _opResult
        set ok [_backpropagate {*}$args]
        set _Caller ""
        set _Callstack [lrange $_Callstack 0 end-1]
        if {$ok==0} {
            logg "Not propagated $this $args" 6
            lappend _EvOrphans [list <-- $this $args]
        }
        return $MID
    }

    method notify2 {filter args} {
        #notifies all obeservers containing $filter in its name
        set obs [searchObs $filter]
        set MID [_createMID]
        set _Caller [namespace which $this]
        lappend _Callstack [list $_Caller $MID]
        foreach o $list {
            $o _notified [namespace which $this] {*}$args
        }
        set _Caller ""
        set _Callstack [lrange $_Callstack 0 end-1]
        return $MID
        #this is the old. buggy version
        #set obs $_Observers
        #set _Observers [searchObs $filter]
        #set r -1
        #if {[llength $_Observers]>0} {
        #    catch {set r [notify {*}$args]}
        #}
        #set _Observers $obs
        #return $r
    }
    method deliver2 {list args} {
        # deliver message directly to listed objects in $list
        # use in conjunction with searchObj/Obs/Children
        if {[llength $args]==0} {return}
        set cmd [lindex $args 0]
        if {$cmd==""} {return}
        set _dspOK($cmd) 0
        
        set MID [_createMID]
        set _Caller [namespace which $this]
        lappend _Callstack [list $_Caller $MID]
        #unset -nocomplain _opResult
        foreach o $list {
            $o _notified [namespace which $this] {*}$args
        }
        set _Caller ""
        set _Callstack [lrange $_Callstack 0 end-1]
        return $MID
        
        #this is the old. buggy version
        #set obs $_Observers
        #set _Observers $list
        #set r -1
        #if {[llength $_Observers]>0} {
        #    catch {set r [notify {*}$args]}
        #}
        #set _Observers $obs
        #return $r
    }
    proc _notifytop {args} {
        # if a _Toplevel is set, notify this one
        # this static proc is used in global context 
        # as needed for bind or after
        # use ::ClOClass::_notifytop
        if {[info exists _Toplevel]} {
            $_Toplevel notify {*}$args
        }
    }
    proc _notifyall {args} {
        # if a _Toplevel is set, notify this one
        # this static proc is used in global context 
        # as needed for bind or after
        # use ::ClOClass::_notifyall
        if {[info exists _Toplevel]} {
            $_Toplevel notifyall {*}$args
        }
    }
    
    method notifytop {args} {
        # if a _Toplevel is set, notify this one
        if {[info exists _Toplevel]} {
            set _Callstack [linsert $_Callstack 0 $this]
            set r [$_Toplevel notify {*}$args]
            set _Callstack [lrange $_Callstack 1 end]
            return $r
        }
    }
    method notifyself {args} {
        # and notify myself as observer
        _notified [namespace which $this] {*}$args
    }
    method notifyall {args} {
        # uses _EvStruct to find ALL Objects that respond to the given Event
        # Can be used as Roundcall or Alarm (e.g. shutdown etc.)
        # Be carefull, unintended sideeffects possible :-)
        if {[llength $args]==0} {return}
        set cmd [lindex $args 0]
        if {$cmd==""} {return}
        
        set evlist [lsearch -index 1 -all $_EvStruct $cmd]
        set olist {}
        foreach ev $evlist {
            set obs [lindex [lindex $_EvStruct $ev] 0]
            lappend olist $obs
        }
        deliver2 $olist {*}$args
    }
    method _garbagecollect_obs {} {
        set garbagecollect 0
        set myobs $_Observers
        foreach inst $_EvStruct {
            lassign $inst obj event method
            lappend myobs $obj
        }
        set myobs [lsort -unique $myobs]
        foreach obs $myobs {
            if {[itcl::find objects $obs]==""} {
                    if {[info commands $obs]==""} {
                        logg "detaching $obs" 10
                        detach $obs
                    }
                }
        }
    }
    method _notify {args} {
        # notifies all observers listed in this object
        if {[llength $args]==0} {return}
        set cmd [lindex $args 0]
        if {$cmd==""} {return}
        set _dspOK($cmd) 0
        foreach obs $_Observers {
            if {[itcl::find objects $obs]==""} {
                if {[info commands $obs]==""} {
                    detach $obs
                    incr garbagecollect
                } else {
                    logg "Note: $obs is not a class, calling proc $obs $cmd" 10
                    _relay $obs {*}$args
                }
            } else {
                set isaclo 0
                catch {set isaclo [$obs isa ClOClass]} e
                
                if {$isaclo} {
                    $obs _notified [namespace which $this] {*}$args
                } else {
                    if {![catch {$obs info function $cmd} e]} {
                        logg "Note: $obs is not a ClO but a class, calling $obs $cmd" 10
                        _relay $obs {*}$args
                    }
                }
                
            }
        }
        if {$garbagecollect>0} {
            _garbagecollect_obs
        }
        return $_dspOK($cmd)
        #
    }
    method _bubble {args} {
        # notify obs level up until someone listens, than break and return
        # will notify all obs in the last level! 
        if {[llength $args]==0} {return}
        set cmd [lindex $args 0]
        if {$cmd==""} {return}
        _notify {*}$args
        
        if {$_dspOK($cmd)==0} {
            foreach obs $_Observers {
                logg "Dispatching bubble $cmd to $obs" 10
                if {$_dspOK($cmd)==0} {
                    $obs _bubble {*}$args
                } else {
                    return $_dspOK($cmd)
                }
            }
        }
        return $_dspOK($cmd)
    }
    method _backpropagate {args} {
        if {[llength $args]==0} {return}
        set cmd [lindex $args 0]
        if {$cmd==""} {return}
        set clients [listClients $this]
        foreach cl $clients {
            $cl _notify {*}$args
            if {$_dspOK($cmd)==0} {
                $cl _backpropagate {*}$args
            } else {
                return $_dspOK($cmd)
            }
        }
        return $_dspOK($cmd)
    }        
    method _notified {caller args} {
        logg "$this notified by $caller $args"  10
        if {[llength $args]==0} {return}
        set cmd [lindex $args 0]
        if {$cmd==""} {return}
        set _Caller $caller
        if {[_delegate $caller {*}$args]>0} {
            incr _dspOK($cmd)
            return
        }
        set _Callstack [linsert $_Callstack 0 $caller]
        if {$_dsplock<32} {#max stack depth hardcoded here.... bad hack
            incr _dsplock
            if {[catch {
                _dispatch {*}$args
            } e]} {
                puts "Dispatch ERROR: $caller $this $args\n$e\n$::errorInfo"
            }
            incr _dsplock -1
        } else {
            puts "$this dropping note $args"
        }
        set _Callstack [lrange $_Callstack 1 end]
        unset -nocomplain _Caller
    }
    method _delegate {caller args} {
        if {[llength $args]==0} {return}

        set cmd [lindex $args 0]
        set delegatee ""
        catch {set delegatee $_Delegates($cmd)}
        if {$delegatee!=""} {
            set delobj [lindex $delegatee 0]
            set deltype [lindex $delegatee 1]
            set alias [lindex $delegatee 2]
            if {$alias ne ""} {
                logg "Note: Delegating cmdname $cmd to $alias calling $delobj" 10
                lset args 0 $alias
                #puts "$args"
            }
            if {$deltype=="variable"} {
                set delobj [$this cget -$delobj]
            }
            set isaclo 0
            catch {set isaclo [$delobj isa ClOClass]} e
            
            if {$isaclo} {
                $delobj _notified $caller {*}$args
            } else {
                logg "OK, $delobj is not a ClO, so calling natively" 6
                _relay $delobj {*}$args
            }
                
            return 1
        }
        return -1
    }
    method _dispatch {args} {
        if {[llength $args]==0} {return}
        
        set cmd0 [lindex $args 0]
        if {$cmd0==""} {return}
        
        set args [lrange $args 1 end]
        set method ""
        catch {set method $_Methods($cmd0)}
        if {$method!=""} {
            set cmd $method
        } else {
            return
        }
        set mbody [$this info function $cmd]
        if {$mbody!=""} {
            set nw "$_Caller --> $cmd0 --> $this"
            logg "Dispatching $nw \n Arguments: $args" 10
            incr _dspOK($cmd0) 
            set v [$this info args $cmd]
            set nvars [llength $v]
            set optionals {}
            foreach varg $v {
                if {[string index $varg 0]=="?"} {
                    #optional arg
                    lappend optionals $varg
                } else {
                    #
                }
            }
            set nargs [llength $args]
            set nopts [llength $optionals]
            set oargs [expr $nvars-$nopts]
            if {$nvars!=$nargs} {
               if {$v=="?arg arg ...?"} {
                   set nvars $nargs
               }
               while {$oargs>$nargs} {
                   lappend args ""
                   incr nargs
               }
               while {$nargs>$nvars} {
                   set args [lrange $args 0 end-1]
                   incr nargs -1
               }
            }
            
            set result [$this $cmd {*}$args]
            
            # retrieve and store result value and meta data messgid {caller cmd args receiver result}
            set cl [lindex $_Callstack end]
            lassign $cl caller mid
            lappend _opResult $mid [list $caller $cmd0 $args [namespace which $this] $result]
            # ok, maxlength of this should be 32 entries
            while {[llength $_opResult]>32} {
                set _opResult [lrange $_opResult 1 end]
            }
            set nw "$caller --> $cmd0 --> $this"
            if {[info exists _NoteWays($nw)]>0} {
                incr _NoteWays($nw)
            } else {
                set _NoteWays($nw) 1
            }
            
        }
    }
    method _relay {obj args} {
        if {[llength $args]==0} {return -1}
        
        set cmd0 [lindex $args 0]
        if {$cmd0==""} {return -1}
        if {[info commands $obj]!=$obj} {return -1}
        
        set args [lrange $args 1 end]
        set cmd $cmd0
        set nw "$_Caller --> $cmd0 --> $obj"
        incr _dspOK($cmd0) 
        logg "Note: _relay calling $obj $cmd $args" 10
            
        set result ""
        if {[catch {set result [$obj $cmd {*}$args]} e]} {
            logg "Error ($obj): $e " 0
        }
            
        # retrieve and store result value and meta data messgid {caller cmd args receiver result}
        set cl [lindex $_Callstack end]
        lassign $cl caller mid
        lappend _opResult $mid [list $caller $cmd0 $args $obj $result]
        # ok, maxlength of this should be 32 entries
        if {[llength $_opResult]>32} {
            set _opResult [lrange $_opResult 1 end]
        }
        set nw "$caller --> $cmd0 --> $obj"
        if {[info exists _NoteWays($nw)]>0} {
            incr _NoteWays($nw)
        } else {
            set _NoteWays($nw) 1
        }
        return 1
    }
    proc getResults {mid} {
        set result {}
        if {[info exists _opResult]} {
            set rl [lsearch -all  $_opResult $mid]
            if {[llength $rl]>0} {
                foreach idx $rl {
                    incr idx
                    set res1 [lindex $_opResult $idx]
                    if {[llength $res1]==5} {
                        lappend result $res1
                    }
                }
            }
        }
        return $result
    }
    proc dump {} {
        chain
        puts "--Struct:"
        set _obsStruct [lsort $_ObsStruct]
        foreach obs $_obsStruct {
            puts "$obs"
        }
        puts "--Events:--"
        set evstruct [lsort $_EvStruct]
        foreach ev $evstruct {
            puts $ev
        }
        set evstruct [lsort -unique $_EvDelegates]
        puts "--Delegated Events:--"
        foreach ev $evstruct {
            puts ">> $ev"
        }
        set evstruct [lsort -unique $_EvExport]
        puts "--Exported Events:--"
        foreach ev $evstruct {
            puts ">> $ev"
        }
        set evstruct [lsort -unique $_SysEvents]
        puts "--System Events:--"
        foreach ev $evstruct {
            puts ">> $ev"
        }
        puts "--Messages:--"
        set noteways [lsort [array names _NoteWays]]
        foreach nw $noteways {
            set c $_NoteWays($nw)
            puts "$c: \t$nw"
        }
        puts "--Orphaned Events:--"
        set evorphans [lsort $_EvOrphans]
        foreach ev $evorphans {
            puts $ev
        }
        puts "--Multiple Bound Events:--"
        set multis [checkDuplicateEvents]
        foreach ev $multis {
            puts $ev
        }
        
    }
    proc define_unknown {} {
        # together with bindEvent ev proc export
        # this will call the global notify chain (_notifytop) 
        # for unknown proc calls that are defined as exported Events 
        # with the Applications ClOClass Eventstructure
        #
        # exampple:
        # class a has 
        # bindEvent test OnTest export
        # ::ClOClass::define_unknown 
        # test
        # --> _notifytop test
        #
        if {$::ClOClass::unknown_original!=""} {
            logg "Warning: unknown is already redefined, exiting" 6
            return 
        }
        uplevel #0 {
            set fname "unknown_[clock seconds]"
            puts "Renaming unknown to $fname"
            rename unknown $fname
            set ::ClOClass::unknown_original  $fname ;
            proc unknown  args  {
                set r {}
                catch {
                    if {[::ClOClass::_boundEvent [lindex $args 0]]>0} {
                        set mid [::ClOClass::_notifytop {*}$args]
                        set r [::ClOClass::getResults $mid]
                    }
                }
                if {[llength $r]>0} {
                    #got handled
                    set r [lindex $r 0]
                    set l [llength $r]
                    incr l -1
                    set res [lindex $r $l]
                    return $res
                }
                tailcall $::ClOClass::unknown_original {*}$args
            }
        }
    }
} 
itcl::configbody ClOClass::garbagecollect {
    if {$garbagecollect>0} {
        _garbagecollect_obs
    }
}
# debug routines, not needed for normal operation
proc __inspect {obj} {
    set obj [uplevel 1 [list namespace which -command $obj]]
    set isa [lmap type {object class metaclass} {
        if {![info object isa $type $obj]} continue
        set type
    }]
    if {"class" in $isa} {
        lappend info {class superclasses} {class mixins} {class filters}
        lappend info {class methods} {class methods}
        lappend info {class variables} {class variables}
    }
    if {"object" in $isa} {
        lappend info {object class} {object mixins} {object filters}
        lappend info {object methods} {object methods}
        lappend info {object variables} {object variables}
        lappend info {object namespace} {object vars}
    }
    set result [dict create isa $isa]
    foreach args $info {
        dict set result $args [info {*}$args $obj]
        foreach opt {-private -all} {
            catch {
                dict set result [list {*}$args $opt] [info {*}$args $obj $opt]
            }
        }
    }
    dict filter $result value {?*}
}
proc __pdict {d args} {   ;# analogous to parray
    set maxl [::tcl::mathfunc::max {*}[lmap key [dict keys $d] {string length $key}]]
    dict for {key value} $d {
        puts stdout [format "%-*s = %s" $maxl $key $value]
    }
}
proc __itcl_getmem {{dump 0}} {
    set classes [itcl::find objects *]
    set rs "Memory Dump\n"
    foreach obj $classes {
        set varlist [$obj configure]
        set size 0
        set r ""
        foreach var $varlist {
            set lsize [string length $var]
            set vname [lindex $var 0]
            if {[array exists $vname]==1} {
                set lsize [string length [array get $vname]
            }
            incr size $lsize
            append r "$obj :: $vname: $lsize\n"
        }
        append rs "Sum: $obj :: $size\n"
        append rs "$r\n"
    }
    set varlist [info vars ::*]
    set size 0
    set r ""
    foreach var $varlist {
        set lsize [string length $$var]
        set vname $var
        if {[array exists $vname]==1} {
            set lsize [string length [array get $vname]]
        }
        incr size $lsize
        append r "    :: $vname: $lsize\n"
    }
    append rs "Sum: Global :: $size\n"
    append rs "$r\n"

    if {$dump>0} {puts $rs}
    return $rs    
}            

