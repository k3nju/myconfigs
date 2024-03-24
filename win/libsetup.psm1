Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# debug
Set-Alias -Name pp -Value Write-Host


function Define-Global($name, $value){
	Set-Variable -Name $name -Value $value -Option ReadOnly -Scope global -Force
}


# HACK: using "[System.Web.Script.Serialization.JavaScriptSerializer]::new()" in method will fail
Add-Type -AssemblyName System.Web.Extensions
Define-Global _JSSerDe ([System.Web.Script.Serialization.JavaScriptSerializer]::new())


function New-PathLayout{
	param([string]$baseDir)
	
	$pl = [PSCustomObject]@{
		BaseDir = $baseDir
		LogFilePath = Join-Path $baseDir "log.txt"
		ArgsFilePath = Join-Path $baseDir "args.json"
		TaskDataFilePath = Join-Path $baseDir "taskdata.json"
		StateFilePath = Join-Path $baseDir "states.json"
	}

	foreach($m in $pl | Get-Member){
		if(-not $m.MemberType -eq "NoteProperty"){
			continue
		}
		if(-not $m.Name.EndsWith("Dir")){
			continue
		}

		New-Item -Path $pl.($m.Name) -ItemType directory -Force | Out-Null
	}

	return $pl
}


function Init-LibSetup{
	param([string]$baseDir,
			[Credential]$userCred)
	
	Define-Global PL (New-PathLayout $baseDir)
	Define-Global Logger ([Logger]::new($global:PL.LogFilePath))
	Define-Global UserCred $userCred
	
	Define-Global ExecCtx ([ExecutionContext]::new())
	Define-Global Opts ([OptionParser]::new($global:Args))
	Define-Global ArgsFile ([DataStore]::new($global:PL.ArgsFilePath))

	if($global:ExecCtx.RunMode() -eq [RunMode]::SHELL){
		$global:ArgsFile.SetRaw("initialArgs", $global:Args)
		$global:ArgsFile.Set("username", $global:UserCred.Username())
		$global:ArgsFile.Save()
	}else{
		# NOTE: overwrite initial(first kick) args for cascading execution
		# e.g: initial: -a b -c d
		#      2nd: -a B -d e
		#      2nd result: -a B -c d -d e
		$initialArgs = $global:ArgsFile.GetRaw("initialArgs")
		$global:Opts.ParseOverride($initialArgs)

		# replace UserCred to first ran
		Define-Global UserCred ([Credential]::new($global:ArgsFile.Get("username")))
	}

	Define-Global TaskData ([DataStore]::new($global:PL.TaskDataFilePath))
	Define-Global StateFile ([DataStore]::new($global:PL.StateFilePath))
	Define-Global TaskExecutor ([TaskExecutor]::new())
}


class Credential{
	# TODO: use WindowsIdentity
	hidden [string]$username_
	hidden [string]$password_ = ""

	hidden [string] NormalizeUsername([string]$name){
		$name = $name.ToLower()
		if($name -in ("nt authority\system", "system")){
			return "system"
		}
		if($name.Contains("\")){
			return $name
		}

		return "$($env:USERDOMAIN.ToLower())\$name"
	}

	Credential([string]$username){
		$this.username_ = $this.NormalizeUsername($username)
	}

	Credential([string]$username, [string]$password){
		$this.username_ = $this.NormalizeUsername($username)
		$this.password_ = $password
	}

	Credential([System.Management.Automation.PSCredential]$cred){
		$this.username_ = $cred.UserName
		$this.password_ = $cred.GetNetworkCredential().Password
	}

	[string] Username(){return $this.username_}
	[string] Password(){return $this.password_}
	[bool] IsSystem(){return $this.username_ -eq "system"}
}


enum LogSeverity{
	DEBUG
	INFO
	ERROR
	EXCEPTION
}

class Logger{
	hidden [string]$filePath_
	hidden [string]$name_ = "Default"
	
	Logger([string]$logFilePath){
		$this.filePath_ = $logFilePath
	}

	Logger([string]$logFilePath, [string]$name){
		$this.filePath_ = $logFilePath
		$this.name_ = $name
	}
	
	[Logger] Clone([string]$name){
		return [Logger]::new($this.filePath_, $name)
	}
	
	[string] Name(){
		return $this.name_
	}
	
	hidden [string] GetColorBySeverity([LogSeverity]$severity){
		switch($severity){
			([LogSeverity]::DEBUG){
				return "gray"
			}
			([LogSeverity]::INFO){
				return "green"
			}
			([LogSeverity]::ERROR){
				return "red"
			}
			([LogSeverity]::EXCEPTION){
				return "red"
			}
		}
		throw "unexpected log severity was passed: $severity"
	}

	[void] Write([string]$msg, [LogSeverity]$severity){
		$now = Get-Date -Format "yyyy-MM-dd hh:mm:ss|"
		$prefix = $now + "$severity|$($this.name_)|"
		($prefix + $msg) | Add-Content -Path $this.filePath_ -Encoding UTF8
		
		Write-Host $prefix -NoNewLine
		Write-Host $msg -Foreground $this.GetColorBySeverity($severity)
	}

	[void] Debug([string]$msg){
		$this.Write($msg, [LogSeverity]::DEBUG)
	}

	[void] Info([string]$msg){
		$this.Write($msg, [LogSeverity]::INFO)
	}

	[void] Error([string]$msg){
		$this.Write($msg, [LogSeverity]::ERROR)
	}

	[void] Exception([string]$msg, $e){
		if($e -is [Exception]){
			$msg = $msg + "`n" + $e.Message
		}
		elseif($e -is [System.Management.Automation.ErrorRecord]){
			$msg = $msg + "`n" + $e.Exception.Message + "`n" + $e.ScriptStackTrace
		}
		else{
			$msg = $msg + "`n" + "`$e=$e"
		}
		
		$this.Write($msg, [LogSeverity]::EXCEPTION)
	}

	[void] Exception([string]$msg){
		$this.Exception($msg, (Get-Variable PSItem -ValueOnly -ErrorAction SilentlyContinue))
	}
}


enum RunMode{
	SHELL # first kick by user
	IMMEDIATE # continuation
	STARTUP # run on startup
	LOGON # run on user logged on
}

class RerunRequest{
	[Credential]$cred_
	[bool]$reboot_
	[RunMode]$mode_

	RerunRequest([Credential]$cred, [bool]$reboot){
		$this.cred_ = $cred
		$this.reboot_ = $reboot
		
		if($this.reboot_){
			if($this.cred_.IsSystem()){
				$this.mode_ = [RunMode]::STARTUP
			}else{
				$this.mode_ = [RunMode]::LOGON
			}
		}else{
			$this.mode_ = [RunMode]::IMMEDIATE
		}
	}
	
	[Credential] Credential(){
		return $this.cred_
	}
	
	[RunMode] RunMode(){
		return $this.mode_
	}
	
	[bool] IsRebootRequired(){
		return $this.reboot_
	}
}


class ExecutionContext{
	hidden [Logger]$logger_
	hidden [RerunRequest]$rerunReq_ = $null
	hidden [bool]$allTaskFinished_ = $false
	
	ExecutionContext(){
		$this.logger_ = $global:Logger.Clone("ExecutionContext")
		$this.CancelRerun()
	}

	[RunMode] RunMode(){
		$mode = $global:Opts.GetOption("runmode", [RunMode]::SHELL)
		return [RunMode]$mode
	}

	# define Creredential for future work
	[Credential] Credential(){
		$name = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name
		return [Credential]::new($name)
	}

	[void] RequireRerun([RerunRequest]$rerunReq){
		$mode = $rerunReq.RunMode()
		$user = $rerunReq.Credential().Username()
		$this.logger_.Info("rerun required: mode=$mode user=$user")
		$this.rerunReq_ = $rerunReq

		if($this.rerunReq_.IsRebootRequired()){
			$this.logger_.Info("reboot required") 
		}
	}
	
	[bool] IsRerunRequired(){
		return $this.rerunReq_ -ne $null
	}

	[bool] IsRebootRequired(){
		if(-not $this.IsRerunRequired()){
			return $false
		}
		return $this.rerunReq_.IsRebootRequired()
	}

	[Status] ScheduleRerun(){
		if((-not $this.IsRerunRequired()) -and (-not $this.IsRebootRequired())){
			return Ng "no need to rerun" "not required neither reboot or rerun"
		}

		try{
			$action = New-ScheduledTaskAction -Execute "powershell.exe" `
				-Argument "-ep bypass -file $($MyInvocation.PSCommandPath) -runmode $($this.rerunReq_.RunMode())"
			$settings = New-ScheduledTaskSettingsSet -RestartCount 5 -RestartInterval (New-TimeSpan -Minutes 1) `
				-AllowStartIfOnBatteries -DontStopIfGoingOnBatteries `
				-StartWhenAvailable `
				-MultipleInstances IgnoreNew
			
			switch($this.rerunReq_){
				{$_.RunMode() -eq [RunMode]::STARTUP}{
					# reboot:  true
					# trigger: startup
					# priv:    system
					$trigger = New-ScheduledTaskTrigger -AtStartup
					$principal = New-ScheduledTaskPrincipal -UserId SYSTEM -RunLevel Highest
					Register-ScheduledTask -TaskName libsetup -Action $action -Settings $settings `
						-Trigger $trigger -Principal $principal
				}
				{$_.RunMode() -eq [RunMode]::LOGON}{
					# reboot:  true
					# trigger: user logged on
					# priv:    user
					$trigger = New-ScheduledTaskTrigger -AtLogOn -User $_.Credential().Username()
					$principal = New-ScheduledTaskPrincipal -UserId $_.Credential().Username() -RunLevel Highest -LogonType S4U
					Register-ScheduledTask -TaskName libsetup -Action $action -Settings $settings `
						-Trigger $trigger -Principal $principal
				}
				{$_.RunMode() -eq [RunMode]::IMMEDIATE}{
					# reboot:  false
					# trigger: N/A
					# priv:    system or user
					$principal = New-ScheduledTaskPrincipal -UserId $_.Credential().Username() -RunLevel Highest
					Register-ScheduledTask -TaskName libsetup -Action $action -Settings $settings -Principal $principal
				}
				default{
					throw "unexpected rerun request: RunMode: $($_.RunMode())"
				}
			}
			return Ok
		}
		catch{
			$this.logger_.Exception("failed to register scheduled task")
			return Ng "failed to register scheduled task"
		}
	}
	
	[void] CancelRerun(){
		foreach($name in @("libsetup", "libsetup_reboot")){
			Unregister-ScheduledTask -TaskName $name -Confirm:$false -ErrorAction SilentlyContinue
		}
	}

	[void] Rerun(){
		if($this.IsRebootRequired()){
			$at = (Get-Date).AddSeconds(10)
			$this.logger_.Info("reboot at $at")
			
			$action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "-c Restart-Computer -Force"
			$trigger = New-ScheduledTaskTrigger -Once -At $at
			$principal = New-ScheduledTaskPrincipal -UserId SYSTEM -RunLevel Highest
			$settings = New-ScheduledTaskSettingsSet -RestartCount 5 -RestartInterval (New-TimeSpan -Minutes 1) -StartWhenAvailable
			Register-ScheduledTask -TaskName libsetup_reboot -Action $action `
				-Trigger $trigger -Principal $principal -Settings $settings
			return
		}
		
		$this.logger_.Info("starting new task")
		Start-ScheduledTask -TaskName libsetup
	}
}


class DataStore{
	hidden [string]$filePath_
	hidden [hashtable]$data_ = @{}

	DataStore([string]$filePath){
		$this.filePath_ = $filePath
		if(Test-Path $this.filePath_){
			$this.Load()
		}
	}

	[void] Load(){
		$this.data_ = $global:_JSSerDe.Deserialize((Get-Content -Path $this.filePath_), [hashtable])
	}

	[void] DebugPrint(){
		$x = $this.data_ | ConvertTo-Json
		pp $x
	}

	[void] Save(){
		$this.data_ | ConvertTo-Json | Out-File -FilePath $this.filePath_
	}

	[string] Get([string]$key){
		if(-not $this.data_.Contains($key)){
			throw "key $key not contained. file: $(this.filePath_)"
		}

		return $this.data_[$key]
	}

	[string] Get([string]$key, [string]$alt){
		try{
			return $this.Get($key)
		}
		catch{
			return $alt
		}
	}

	[void] Set([string]$key, [string]$value){
		$this.data_[$key] = $value
	}

	[object] GetRaw([string]$key){
		if(-not $this.data_.Contains($key)){
			throw "key $key not contained. file: $(this.filePath_)"
		}
		
		return $this.data_[$key]
	}

	[object] GetRaw([string]$key, [object]$alt){
		try{
			return $this.GetRaw($key)
		}
		catch{
			return $alt
		}
	}

	[void] SetRaw([string]$key, [object]$value){
		$this.data_[$key] = $value
	}

	[void] Remove([string]$key){
		if($this.data_.Contains($key)){
			$this.data_.Remove($key)
		}
	}

	[void] Clear(){
		$this.data_ = @{}
	}
}

class Status{
	hidden [bool]$result_
	hidden [object]$retval_
	hidden [string]$errMsg_
	hidden [string]$reason_

	Status([bool]$result, [object]$retval, [string]$errmsg, [string]$reason){
		$this.result_ = $result
		$this.retval_ = $retval
		$this.errMsg_ = $errmsg
		$this.reason_ = $reason
	}

	[bool] Ok(){return $this.result_}
	[object] RetVal(){return $this.retval_}
	[string] Error(){
		if($this.result_){
			return ""
		}
		return "ERROR: $($this.errMsg_): $($this.reason_)"
	}
}

function Ok{
	param([object]$retval = $null)
	return [Status]::new($true, $retval, "", "")
}

function Ng{
	param(
		[string]$errmsg,
		[string]$reason = ""
	)
	return [Status]::new($false, $null, $errmsg, $reason)
}


#NOTE: order by preceding
enum TaskState{
	FAILED
	EXECUTED
	RERUN
	UNEXECUTED
}

# responsible to CRUD task states per tasks
class TaskStateMarker{
	hidden [string]$taskName_
	
	TaskStateMarker([string]$taskName){
		$this.taskName_ = $taskName
	}

	[TaskState] CurrentState(){
		return $global:StateFile.Get($this.taskName_, [TaskState]::UNEXECUTED)
	}

	[void] Mark([TaskState] $state){
		$global:StateFile.Set($this.taskName_, $state.ToString())
		$global:StateFile.Save()
	}

	[void] Clear(){
		$global:StateFile.Remove($this.taskName_)
		$global:StateFile.Save()
	}
}


enum TaskResult{
	ERROR
	OK
	REBOOT
	# from a perspective of a task, RERUN always involves a reboot
	RERUN
}

class TaskBase{
	hidden [string]$name_
	hidden [Credential]$cred_
	hidden [TaskStateMarker]$state_
	# without trailing _ means, logger is a protected member
	hidden [Logger]$logger
	
	
	TaskBase(){
		$this.Init($this.ToString(), $null)
	}

	TaskBase([string]$name){
		$this.Init($name, $null)
	}

	TaskBase([string]$name, [Credential]$execCred){
		$this.Init($name, $execCred)
	}
	
	hidden [void]Init([string]$name, [Credential]$execCred){
		if($execCred -eq $null){
			$execCred = [Credential]::new([System.Security.Principal.WindowsIdentity]::GetCurrent().Name, "")
		}
		
		$this.name_ = $name
		$this.cred_ = $execCred
		$this.state_ = [TaskStateMarker]::new($this.name_)
		$this.logger = $global:Logger.Clone($this.name_)
	}

	[string] Name(){return $this.name_}
	[Credential] Credential(){return $this.cred_}
	[bool] IsRunnable(){return $true}

	[TaskState] State(){return $this.state_.CurrentState()}
	[void] ResetState(){$this.state_.Clear()}

	[TaskResult] EnsureTaskResultReturned($result){
		if($result -is [array] -and $result.Length -gt 0){
			$result = $result[-1]
		}
		if($result -eq $null){
			$result = [TaskResult]::OK
		}
		if($result -isnot [TaskResult]){
			throw "unexpected return type: expected TaskResult, but $($result.GetType())"
		}
		return $result
	}
	
	[TaskResult] RunImpl($taskArgs){
		throw "unimplemented"
	}

	[Status] Run($taskArgs){
		try{
			$taskArgs["TASK_DATA"] = $global:TaskData.GetRaw($this.name_, @{})
			$result = $this.RunImpl($taskArgs)
			$result = $this.EnsureTaskResultReturned($result)
			switch($result){
				([TaskResult]::ERROR){
					$this.state_.Mark([TaskState]::FAILED)
					return Ok
				}
				([TaskResult]::OK){
					$this.state_.Mark([TaskState]::EXECUTED)
					return Ok
				}
				([TaskResult]::REBOOT){
					$this.state_.Mark([TaskState]::EXECUTED)
					return Ok ([TaskResult]::REBOOT)
				}
				([TaskResult]::RERUN){
					$this.state_.Mark([TaskState]::RERUN)
					return Ok ([TaskResult]::RERUN)
				}
			}
			throw "unexpected task state: $result"
		}
		catch{
			$this.logger.Exception("exception caught: task $($this.Name())")
			$this.state_.Mark([TaskState]::FAILED)
		}
		finally{
			$global:TaskData.SetRaw($this.name_, $taskArgs["TASK_DATA"])
			$global:TaskData.Save()
			$taskArgs.Remove("TASK_DATA")
		}
		
		return Ng "task failed" "unexpected execution path reached"
	}
}


class AdhocTask : TaskBase{
	# member variable for passing taskArgs to script block codes
	hidden [hashtable] $taskArgs

	AdhocTask([string]$name, [string]$username, [ScriptBlock]$script)
	:base($name, [Credential]::new($username)){
		Add-Member -InputObject $this -MemberType ScriptMethod -Name Script -value $script 
	}
	
	[TaskResult] RunImpl($taskArgs){
		try{
			$this.taskArgs = $taskArgs
			$rv = $this.Script()
			$rv = $this.EnsureTaskResultReturned($rv)
			return $rv
		}
		catch{
			$msg = "exception caught: adhoc task $($this.Name())"
			$innerExp = $PSItem.Exception.InnerException
			if($innerExp -eq $null){
				$this.logger.Exception($msg)
			}else{
				$this.logger.Exception($msg, $innerExp)
			}
			return [TaskResult]::ERROR
		}
		throw "never reach here"
	}
}

# task helpers
function Task {
	param(
		[string]$name,
		[ScriptBlock]$script
	)

	$cred = $global:ExecCtx.Credential()
	$global:TaskExecutor.AddTask([AdhocTask]::new($name, $cred.Username(), $script))
}

function UserTask {
	param(
		[string]$name,
		[ScriptBlock]$script
	)

	$global:TaskExecutor.AddTask([AdhocTask]::new($name, $global:UserCred.Username(), $script))
}

function SystemTask {
	param(
		[string]$name,
		[ScriptBlock]$script
	)

	$global:TaskExecutor.AddTask([AdhocTask]::new($name, "system", $script))
}


function Get-This {
	$cs = Get-PSCallStack
	for($i = 0; $i -lt $cs.length; ++$i){
		$fn = $cs[$i].FunctionName
		if($fn -eq "RunImpl"){
			return Get-Variable -Name this -ValueOnly -Scope ($i - 1)
		}
	}
	throw "Info helper could not find AdhocTask.RunImpl callstack"
}


# log helpers
function Info {
	param([Parameter(ValueFromPipeline=$true)][string[]]$lines)
	
	begin{
		$this = Get-This
		$output = @()
	}
	
	process{
		foreach($line in $lines){
			$this.logger.Info($line)
			$output += $line
		}
	}

	end{
		$output
	}
}

function Debug {
	param([Parameter(ValueFromPipeline=$true)][string[]]$lines)
	
	begin{
		$this = Get-This
		$output = @()
	}
	
	process{
		foreach($line in $lines){
			$this.logger.Debug($line)
			$output += $line
		}
	}

	end{
		$output
	}
}

function Cmd{
	param([object]$cmd)

	switch($cmd.GetType()){
		([ScriptBlock]){
			Debug ("cmd:" + $cmd.ToString())
			$output = &$cmd | Debug
			return $output
		}
		([string]){
			Debug ("cmd:" + $cmd)
			$output = Invoke-Expression $cmd | Debug
			return $output
		}
		default{
			throw "Invalid type passed. expected ScriptBLock or string, but $($cmd.GetType()): ${cmd}"
		}
	}
}


class TaskExecutor{
	hidden [System.Collections.Generic.List[TaskBase]]$tasks_ = @()
	hidden [Logger]$logger_
	hidden [int]$nextIndex_

	TaskExecutor(){
		$this.logger_ = $global:Logger.Clone("TaskExecutor")
		$this.nextIndex_ = 0
	}

	[void] AddTask($task){
		$this.logger_.Info("adding task: $($task.Name())")

		$run = $global:Opts.GetOptions("run", $null)
		if(($run -ne $null) -and ($task.Name() -notin $run)){
			$this.logger_.Info("not listed in run options. ignored: $($task.Name())")
			return
		}
		
		$reset = $global:Opts.GetOption("reset", $null)
		if(($reset -eq $true) -or ($reset -eq $task.Name())){
			$task.ResetState()
			$this.logger_.Info("task state is reset: $($task.Name())")
		}

		$this.tasks_.Add($task)
		$this.logger_.Info("added: $($task.Name())")
	}

	[void] ResetState(){
		foreach($task in $this.tasks_){
			$task.ResetState()
			$this.logger_.Info("task state reset: $($task.Name())")
		}
	}

	[Status] IsAllTaskRunnable(){
		foreach($task in $this.tasks_){
			if(($task.State() -in [TaskState]::RERUN, [TaskState]::UNEXECUTED) -and (-not $task.IsRunnable())){
				return Ng "$($task.Name()) is not runnable"
			}
		}
		return Ok
	}

	[int] Remains(){
		$finished = 0
		foreach($task in $this.tasks_){
			$state = $task.State()
			if($state -in ([TaskState]::FAILED, [TaskState]::EXECUTED)){
				$finished += 1
			}
		}

		return $this.tasks_.Count - $finished
	}

	hidden [Status] NextTask(){
		if($this.nextIndex_ -ge $this.tasks_.Count){
			return Ng "out of tasks" ""
		}
		return Ok ($this.tasks_[$this.nextIndex_++])
	}

	hidden [Status] PeekNextTask(){
		$rv = $this.NextTask()
		--$this.nextIndex_
		return $rv
	}

	[void] Run($taskArgs){
		$curCred = $global:ExecCtx.Credential()

		try{
			:taskloop while($true){
				$rv = $this.NextTask()
				if(-not $rv.Ok()){
					$this.logger_.Info("no remaining tasks")
					return
				}

				$task = $rv.RetVal()
				$name = $task.Name()
				switch($task.State()){
					([TaskState]::FAILED){
						$this.logger_.Info("${name}: state is FAILED. skipping")
						continue taskloop
					}
					([TaskState]::EXECUTED){
						$this.logger_.Info("${name}: state is EXECUTED. skipping")
						continue taskloop
					}
					([TaskState]::RERUN){
						$this.logger_.Info("${name}: state is RERUN. re-executing")
					}
					([TaskState]::UNEXECUTED){
						$this.logger_.Info("${name}: state is UNEXECUTED. executing")
					}
					default{
						throw "aborting task executor. unexpected task state found. ${name}: $($task.State())"
					}
				}

				if($curCred.Username() -ne $task.Credential().Username()){
					$cu = $curCred.Username()
					$tu = $task.Credential().Username()
					$this.logger_.Info("${name}: required context is not matched: required=$tu current=$cu")
					$rerunReq = [RerunRequest]::new($task.Credential(), $false)
					$global:ExecCtx.RequireRerun($rerunReq)
					return
				}
				
				if(-not $task.IsRunnable()){
					$this.logger_.Info("$name is not runnable. skipping")
					continue
				}

				$this.logger_.Info("${name}: executing")
				$rv = $task.Run($taskArgs)
				switch($task.State()){
					([TaskState]::FAILED){
						$this.logger_.Error("${name}: failed. state is changed to $($task.State())")
					}
					{$_ -in ([TaskState]::EXECUTED, [TaskState]::RERUN)}{
						$this.logger_.Info("${name}: executed. state is changed to $($task.State())")
					}
					default{
						throw "aborting executor. changed to unexpected task state. ${name}: $($task.State())"
					}
				}
				
				if(-not $rv.Ok()){
					throw "aborting task executor. error returned. ${name}: $($rv.Error())"
				}
				$taskResult = $rv.RetVal()
				
				$nextTask = $null
				switch($taskResult){
					([TaskResult]::REBOOT){
						$rv = $this.PeekNextTask()
						if($rv.Ok()){
							$nextTask = $rv.RetVal()
						}else{
							# tasks exhausted
							$this.logger_.Info("no task remaining but reboot required. enqueueing dummy task")
							# dummy
							$nextTask = [AdhocTask]::new("dummy", "system", {return [TaskResult]::OK})
						}
					}
					([TaskResult]::RERUN){
						$nextTask = $task
					}
					($null){
						continue taskloop
					}
					default{
						throw "aborting task executor. unexpected task result returned. ${name}: ${taskResult}"
					}
				}

				$this.logger_.Info("next task: $($nextTask.Name())")
				$rerunReq = [RerunRequest]::new($task.Credential(), $true)
				$global:ExecCtx.RequireRerun($rerunReq)
				break
			}
		}
		catch{
			$this.logger_.Exception("fatal. exception caught on task executor")
			return
		}
	}
}


# relaxed option parser
# -a -> a:$true
# -a b -> a:b
# -a b -a c -> a:[a,b]
# b -> ignored
class OptionParser{
	hidden [System.Collections.Hashtable]$opts_ = @{}
	hidden [int]$consumed_
	hidden [object[]]$srcArgs_

	hidden [void] Init([object[]]$srcArgs){
		$this.consumed_ = 0
		$this.srcArgs_ = $srcArgs
		$this.opts_ = @{}
	}
	
	OptionParser([object[]]$srcArgs){
		$this.Init($srcArgs)
		$this.Parse()
	}

	hidden [void] Parse(){
		while($this.Remains() -gt 0){
			if($this.Remains() -ge 2){
				if($this.IsKey(0) -and $this.IsKey(1)){
					$this.ParseAsFlag(0)
					continue
				}
				if($this.IsKey(0) -and (-not $this.IsKey(1))){
					$this.ParseAsKeyValue(0)
					continue
				}
				$this.consumed_ += 2
				continue
			}
			
			if($this.IsKey(0)){
				$this.ParseAsFlag(0)
				continue
			}
			$this.consumed_ += 1
		}
	}

	hidden [int] Remains(){
		return $this.srcArgs_.Count - $this.consumed_
	}

	hidden [bool] IsKey($offset){
		$pos = $this.consumed_ + $offset
		if($pos -ge $this.srcArgs_.Count){
			return $false
		}
		return $this.srcArgs_[$pos].StartsWith("-")
	}

	hidden [void] ParseAsFlag($offset){
		$pos = $this.consumed_ + $offset
		if($pos -ge $this.srcArgs_.Count){
			return
		}

		$key = $this.srcArgs_[$pos]
		$key = $key.Substring(1, $key.Length-1).ToLower()
		$this.opts_[$key] = $true
		
		$this.consumed_ += 1
	}

	hidden [void] ParseAsKeyValue($offset){
		$kpos = $this.consumed_ + $offset
		if($kpos -ge $this.srcArgs_.Count){
			return
		}
		$vpos = $this.consumed_ + $offset + 1
		if($vpos -ge $this.srcArgs_.Count){
			return
		}

		$key = $this.srcArgs_[$kpos]
		$key = $key.Substring(1, $key.Length-1).ToLower()
		$val = $this.srcArgs_[$vpos]

		if(-not $this.opts_.Contains($key)){
			$this.opts_[$key] = @()
		}
		$this.opts_[$key] += $val

		$this.consumed_ += 2
	}

	[object] GetOption([string]$key){
		if(-not $this.opts_.Contains($key)){
			throw "option not found: $key"
		}
		return $this.opts_[$key][0]
	}
	
	[object] GetOption([string]$key, $alt){
		try{
			return $this.GetOption($key)
		}
		catch{
			return $alt
		}
	}

	[object] GetOptions([string]$key){
		if(-not $this.opts_.Contains($key)){
			throw "option not found: $key"
		}
		return $this.opts_[$key]
	}
	
	[object] GetOptions([string]$key, $alt){
		try{
			return $this.GetOptions($key)
		}
		catch{
			return $alt
		}
	}

	[void] ParseOverride([object[]]$newArgs){
		# save current opts, parse newArgs and orverride by saved opts
		$saved = $this.opts_
		$this.Init($newArgs)
		
		$this.Parse()

		foreach($k in $saved.Keys){
			$this.opts_[$k] = $saved[$k]
		}

		# -reset is valid for initial run only
		if($this.opts_.Contains("reset")){
			$this.opts_.Remove("reset")
		}
	}

	[string] ToString(){
		$tmp = @()
		foreach($e in $this.opts_.GetEnumerator()){
			$tmp += "-$($e.Key) $($e.Value)"
		}
		return ($tmp -join " ")
	}

	[void] DebugPrint(){
		foreach($e in $this.opts_.GetEnumerator()){
			pp "$($e.Key) $($e.Value)"
		}
	}
}


class ProcLock{
	hidden [System.Threading.Mutex]$mu_ = $null

	ProcLock(){
		$this.mu_ = [System.Threading.Mutex]::new($false, "Global\libsetup_mu")
	}

	[bool] TryLock(){
		while($true){
			try{
				$a = $this.mu_.WaitOne(0)
				return $a
			}
			catch [System.Threading.AbandonedMutexException] {
				continue
			}
			catch{
				throw
			}
		}

		# never
		return $false
	}

	[void] Release(){
		if($this.mu_ -eq $null){
			return
		}
		$this.mu_.ReleaseMutex()
		$this.mu_ = $null
	}
}


class Setup{
	hidden [ProcLock]$lock_
	hidden [Logger]$logger_

	hidden [void] Lock(){
		$this.lock_ = [ProcLock]::new()
		if(-not $this.lock_.TryLock()){
			throw "process lock failed: another task is running"
		}
	}

	hidden [void] Unlock(){
		$this.lock_.Release()
	}
	
	Setup([string]$baseDir, [Credential]$userCred){
		$this.Lock()
		$this.Init($baseDir, $userCred)
	}

	Setup([string]$baseDir, [string]$username){
		$this.Lock()
		$this.Init($baseDir, [Credential]::new($username))
	}

	hidden [void] Init([string]$baseDir, [Credential]$userCred){
		Init-LibSetup $baseDir $userCred
		$this.logger_ = $global:Logger.Clone("Setup")
		$this.logger_.Info("initialized")
	}

	Run(){
		try{
			$this.logger_.Info("setup started: mode=$($global:ExecCtx.RunMode()) user=$($global:ExecCtx.Credential().Username())")
			$this.logger_.Info("Args: $($global:Opts.ToString())")
			
			# TODO: enhance taskArgs
			# e.g.: set execution id, rerun count
			$taskArgs = @{}
			$global:TaskExecutor.Run($taskArgs)

			if($global:ExecCtx.IsRerunRequired()){
				$rv = $global:ExecCtx.ScheduleRerun()
				if(-not $rv.Ok()){
					throw "scheduling rerun failed: $($rv.Error())"
				}
				$this.logger_.Info("rerun scheduled")
				
				$global:ExecCtx.Rerun()
				return
			}

			$this.logger_.Info("all tasks finished")
		}
		catch{
			$this.logger_.Exception("fatal. exception caught")
		}
		finally{
			$this.Unlock()
		}
	}
}
