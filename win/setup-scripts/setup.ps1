Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"


Set-Alias -Name pp -Value Write-Host


# define readonly globals
function Define-Global($name, $value){
		Set-Variable -Name $name -Value $value -Option ReadOnly -Scope global -Force
}

Define-Global ChocoEssentialPackages @(
		"sysinternals"
		,"hxd"
		,"stirling-jp"
		,"x64dbg.portable"
)

Define-Global WingetEssentialPackages @(
		"GNU.Emacs"
		,"voidtools.Everything"
		,"Google.Chrome"
		,"MSYS2"
		,"7zip.7zip"
)



# path aggregation
class PathLayout{
		$SetupScriptDir = "c:/setup-scripts/"
		# $this works. how insane.
		$ResourcesDir = (Join-Path $this.SetupScriptDir "resources")
		$StateFilePath = (Join-Path $this.SetupScriptDir "states.json")
		$TmpDir = "c:/tmp/"
		$LogFilePath = "c:/setup-scripts-log.txt"

		PathLayout(){
				foreach($m in [PathLayout].GetMembers()){
						if($m.MemberType -ne "Property"){
								continue
						}
						if(-not $m.Name.EndsWith("Dir")){
								continue
						}

						New-Item -Path ($this.($m.Name)) -ItemType directory -Force
				}
		}
}
Define-Global PL ([PathLayout]::New())


enum LogSeverity{
		INFO
		ERROR
		EXCEPTION
}


# logger
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
				return [Logger]::New($this.filePath_, $name)
		}

		hidden [string]GetColorBySeverity([LogSeverity]$severity){
				switch($severity){
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

		[void] Info([string]$msg){
				$this.Write($msg, [LogSeverity]::INFO)
		}

		[void] Error([string]$msg){
				$this.Write($msg, [LogSeverity]::ERROR)
		}

		[void] Exception([string]$msg){
				$this.Write($msg + "`n" +
										$PSItem.Exception.Message + "`n" +
										$PSItem.ScriptStackTrace + "`n",
										[LogSeverity]::EXCEPTION)
		}
}
Define-Global Logger ([Logger]::new($global:PL.LogFilePath))


# file ops abstraction  
class DataStore{
		hidden [string]$filePath_
		hidden [System.Collections.Specialized.OrderedDictionary]$data_ = @{}

		DataStore([string]$filePath){
				$this.filePath_ = $filePath
				if(Test-Path $this.filePath_){
						$this.Load()
				}
		}

		[void] Load(){
				$pso = Get-Content -Path $this.filePath_ | ConvertFrom-Json
				[System.Collections.Specialized.OrderedDictionary]$tmp = @{}
				foreach($prop in $pso.PSObject.Properties){
						$tmp[$prop.Name] = $prop.Value
				}
				$this.data_ = $tmp
		}

		[void] Save(){
				$this.data_ | ConvertTo-Json | Out-File -FilePath $this.filePath_
		}

		[string] Get([string]$key){
				if(-not $this.data_.Contains($key)){
						throw "key $key not contained"
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

		[void] Remove([string]$key){
				if($this.data_.Contains($key)){
						$this.data_.Remove($key)
				}
		}

		[void] Clear(){
				$this.data_ = @{}
		}
}
Define-Global StateFile ([DataStore]::new($global:PL.StateFilePath))


#NOTE: order by preceding
enum TaskState{
		FAILED
		EXECUTED
		RERUN
		UNEXECUTED
}

<#
class TaskStateMarker{
		hidden [string] $basePath_

		TaskStateMarker([string]$taskName){
				$this.basePath_ = Join-Path $global:PL.TaskStateDir ($taskName + ".")
		}

		hidden [string] FindStateFile(){
				foreach($stateName in [Enum]::GetNames([TaskState])){
						$path = $this.basePath_ + $stateName
						if(Test-Path $path){
								return $path
						}
				}
				return $null # NOTE: $null becomes empty string
		}

		hidden [void] RemoveStateFile(){
				$path = $this.FindStateFile()
				if(($path -ne "") -and (Test-Path $path)){
						Remove-Item $path
				}
		}

		hidden [TaskState] StateFileToState(){
				$path = $this.FindStateFile()
				if($path -eq ""){
						return [TaskState]::UNEXECUTED
				}

				$ext = [System.IO.Path]::GetExtension($path)
				$ext = $ext.SubString(1, $ext.Length-1)
				
				return [TaskState]$ext
		}

		[TaskState] CurrentState(){
				return $this.StateFileToState()
		}

		[void] Mark([TaskState] $state){
				$src = $this.FindStateFile()
				$dst = $this.basePath_ + $state.ToString()
				if($src -eq ""){
						New-Item -Path $dst
						return
				}
				Rename-Item -Path $src -New $dst
		}

		[void] Clear(){
				$path = $this.FindStateFile()
				if($path -ne ""){
						Remove-Item $path
				}
		}
}
#>

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


# status wraps result of function call 
class Status{
		hidden [bool]$result_
		hidden [object]$retval_
		hidden [string]$errorMsg_
		hidden [string]$reason_

		Status([bool]$result, [object]$retval, [string]$errmsg, [string]$reason){
				$this.result_ = $result
				$this.retval_ = $retval
				$this.errorMsg_ = $errmsg
				$this.reason_ = $reason
		}

		[bool] Ok(){return ($this.result_ -eq $true)}
		[object] Value(){return $this.retval_}
		[string] ErrorMessage(){return $this.errorMsg_}
		[string] Reason(){return $this.reason_}
}
function Ok{
		param (
				[object]$retval = $true
		)
		return [Status]::new($true, $retval, "", "")
}
function Ng{
		param (
				[string]$errmsg,
				[string]$reason = ""
		)
		return [Status]::new($false, $null, $errmsg, $reason)
}


# TaskResult represents a result of task process.
# NOTE: TaskResult is status of task process.
#       not describing task state.
enum TaskResult{
		ERROR
		OK
		RERUN
}


# base class for task
class TaskBase{
		hidden [string] $name_
		hidden [TaskStateMarker]$state_
		# without trailing _ means, logger is a protected member
		hidden [Logger]$logger

		TaskBase(){
				$this.Init($this.ToString())
		}

		TaskBase([string]$name){
				$this.Init($name)
		}

		hidden [void] Init([string]$name){
				$this.name_ = $name
				$this.state_ = [TaskStateMarker]::New($this.name_)
				$this.logger = $global:Logger.Clone($this.name_)
				
		}

		[string]Name(){
				return $this.name_
		}

		[bool] Runnable(){
				return $true
		}

		[TaskResult] RunImpl($taskArgs){
				throw "unimplemented"
		}

		[Status] Run($taskArgs){
				try{
						$result = $this.RunImpl($taskArgs)
						switch($result){
								([TaskResult]::ERROR){
										$this.state_.Mark([TaskState]::FAILED)
										return Ng "task failed: $($this.Name())" "returned [TaskResult]::ERROR"
								}
								([TaskResult]::OK){
										$this.state_.Mark([TaskState]::EXECUTED)
								}
								([TaskResult]::RERUN){
										$this.state_.Mark([TaskState]::RERUN)
								}
								default{
										throw "unexpected task state: $result"
								}
						}
						return Ok $result
				}
				catch{
						$this.logger.Exception("exception caught: task $($this.Name())")
						$this.state_.Mark([TaskState]::FAILED)
						return Ng "exception caught" $PSItem.Exception.Message
				}
		}

		[TaskState] State(){
				return $this.state_.CurrentState()
		}

		[void] ResetState(){
				$this.state_.Clear()
		}
		
		<#
		[void] MarkAsFailed(){
				$this.state_.Mark([TaskState]::FAILED)
		}
		
		[void] MarkAsExecuted(){
				$this.state_.Mark([TaskState]::EXECUTED)
		}
		
		[void] MarkAsRerun(){
				$this.state_.Mark([TaskState]::RERUN)
		}

		[void] Info([string]$msg){
				$this.logger_.Info($msg)
		}

		[void] Error([string]$msg){
				$this.logger_.Error($msg)
		}

		[void] Exception([string]$msg){
				$this.logger_.Exception($msg)
		}
    #>
}


# task executor. aggregates tasks
class TaskExecutor{
		#hidden [System.Collections.Hashtable]$tasks = @{}
		hidden [System.Collections.Specialized.OrderedDictionary]$tasks_ = @{}
		hidden [Logger]$logger_

		TaskExecutor(){
				$this.logger_ = $global:Logger.Clone("TaskExecutor")
		}

		[void] AddTask($task){
				$this.tasks_.Add($task.Name(), $task)
		}

		[void] RemoveTask($taskName){
				$this.tasks_.Remove($taskName)
		}

		[Status] IsAllTaskRunnable(){
				foreach($name in $this.tasks_.Keys){
						$task = $this.tasks_[$name]
						if(-not $task.Runnable()){
								return Ng "$task is not runnable"
						}
				}
				return Ok
		}

		[void] Run($taskArgs){
				:taskloop foreach($name in $this.tasks_.Keys){
						try{
								$task = $this.tasks_[$name]
								switch($task.State()){
										([TaskState]::FAILED){
												$this.logger_.Info("task $name is marked as failed. skipping")
												continue taskloop
										}
										([TaskState]::EXECUTED){
												$this.logger_.Info("task $name is marked as executed. skipping")
												continue taskloop
										}
										([TaskState]::RERUN){
												$this.logger_.Info("task $name is marked as rerun. re-executing")
										}
										([TaskState]::UNEXECUTED){
												$this.logger_.Info("task $name is marked as unexecuted. executing")
										}
										default{
												$this.logger_.Info("task $name is unknown state. aborting")
												throw "unexpected task state found: $name"
										}
								}
								
								if(-not $task.Runnable()){
										$this.logger_.Info("task $name is not runnable. skipping")
										continue
								}
								
								$this.logger_.Info("executing task: $name")
								$status = $task.Run($taskArgs)
								if($status.Ok() -and $status.Value() -eq [TaskState]::RERUN){
										$global:ExecCtx.Rerun()
										return
								}
								$this.logger_.Info("task executed: $name")
						}
						catch{
								$this.logger_.Exception("fatal. exception caught: $name")
						}
				}
		}
}


# update powershell environments and install basic ps modules
class UpdatePowershellEnv : TaskBase {
		[TaskResult] RunImpl($taskArgs){
				$this.logger.Info("updating NuGet provider")
				Install-PackageProvider -Name NuGet -MinimumVersion 2.8.5.201 -Force

				$this.logger.Info("trusting PSGallery repository")
				Set-PSRepository -Name PSGallery -InstallationPolicy Trusted

				$this.logger.Info("installing PSWindowsUpdate")
				Install-Module -Name PSWindowsUpdate -Force

				return [TaskResult]::OK
		}
}


# run windows update
class WindowsUpdate : TaskBase{
		[TaskResult] RunImpl($taskArgs){
				$this.logger.Info("running windows update")
				Import-Module PSWindowsUpdate
				Install-WindowsUpdate -AcceptAll -IgnoreReboot

				$rebootRequired = Get-WURebootStatus -Silent
				if($rebootRequired){
						$this.logger.Info("windows reboot required")
						return [TaskResult]::RERUN
				}
				
				$this.logger.Info("windows reboot is not required")
				return [TaskResult]::OK
		}
}


# populate predefined generic registry configs
class PopulateRegistryConfig : TaskBase{
		[TaskResult] RunImpl($taskArgs){
				if($this.State() -eq [TaskState]::RERUN){
						$this.logger.Info("task state is RERUN that seems windows has been rebooted")
						return [TaskResult]::OK
				}
				
				$this.logger.Info("populating reg files")
				$regFiles = Get-ChildItem -Path $global:PL.ResourcesDir -Filter "*.reg"
				foreach($regFile in $regFiles){
						$this.logger.Info("applying reg file: $($regFile.FullName)")
						regedit /s $regFile.FullName
				}
				
				return [TaskResult]::RERUN
		}
}


# delete default shortcuts
class DeleteDefaultShortcuts : TaskBase{
		[TaskResult] RunImpl($taskArgs){
				$this.logger.Info("deleting default shortcuts")
				$lnks = Get-ChildItem $env:Public\Desktop\*.lnk
				foreach($lnk in $lnks){
						$this.logger.Info("deleting: $lnk")
						Remove-Item $lnk -Force
				}

				return [TaskResult]::OK
		}
}


# clean taskbar items pinned
class CleanTaskbar : TaskBase{
		hidden [string]$path_ = "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Taskband"

		[TaskResult] RunImpl($taskArgs){
				$this.logger.Info("cleaning taskbar pinneds")
				
				if(Test-Path $this.path_){
						$this.logger.Info("deleting: $($this.path_)")
						Remove-Item $this.path_ -Recurse -Force
				}
				
				return [TaskResult]::OK
		}
}


# enable RDP
class EnableRDP : TaskBase{
		hidden [string]$path_ = "HKLM:SYSTEM\CurrentControlSet\Control\Terminal Server"
		hidden [string]$key_ = "fDenyTSConnections"

		[TaskResult] RunImpl($taskArgs){
				$this.logger.Info("enabling RDP")

				$this.logger.Info("setting $this.path_\$this.key_ to 0")
				Set-ItemProperty `
					-Path $this.path_ `
					-Name $this.key_ `
					-Value 0 `
					-Force

				$this.logger.Info("enabling RDP firewall rules")
				foreach($rule in Get-NetFirewallRule){
						if($rule.Name -like "RemoteDesktop-UserMode-IN-*"){
								$this.logger.Info("enabling: $rule.Name")
								Enable-NetFirewallRule -Name $rule.Name
						}
				}

				return [TaskResult]::OK
		}
}


# enable openssh
class EnableOpenSSH : TaskBase{
		[TaskResult] RunImpl($taskArgs){
				$rebootRequired = $false

				$this.logger.Info("installing OpenSSH")
				$caps = Get-WindowsCapability -Online -Name "*openssh*"
				foreach($cap in $caps){
						if($cap.State -ne "installed"){
								$this.logger.Info("installing: $($cap.Name)")
								$ret = Add-WindowsCapability -Online -Name $cap.Name
								$rebootRequired += $ret.RestartNeeded
						}
				}
				
				if($rebootRequired){
						$this.logger.Info("windows reboot required")
						return [TaskResult]::RERUN
				}

				$this.logger.Info("setting default shell to bash")

				New-ItemProperty `
					-Path HKLM:\SOFTWARE\OpenSSH `
					-Name DefaultShell `
					-Value c:\windows\system32\bash.exe `
					-PropertyType String `
					-Force

				$this.logger.Info("enabling OpenSSH")
				$sshd = Get-Service -Name sshd
				Set-Service -Name $sshd.Name -StartupType automatic
				Start-Service -Name $sshd.Name
				
				$this.logger.Info("windows reboot is not required")
				return [TaskResult]::OK
		}
}


# package [un]installation base
class PackageInstallerBase : TaskBase{
		# override below from deriveds
		hidden [string[]] $installPackages = @()
		hidden [string[]] $uninstallPackages = @()

		hidden [string[]] ListInstallPackages(){
				return @()
		}

		hidden [string[]] ListUninstallPackages(){
				return @()
		}

		hidden [string] FormatInstallCommand($pkgName){
				throw "unimplemented"
		}

		hidden [string] FormatUninstallCommand($pkgName){
				throw "unimplemented"
		}
		
		hidden [Status] Install($pkgName){
				try{
						$cmd = $this.FormatInstallCommand($pkgName)
						$this.logger.Info($cmd)
						Invoke-Expression $cmd
						return Ok
				}
				catch{
						return Ng "install failed: $pkgName" $PSItem.Exception.Message
				}
		}

		hidden [Status] Uninstall($pkgName){
				try{
						$cmd = $this.FormatUninstallCommand($pkgName)
						$this.logger.Info($cmd)
						Invoke-Expression $cmd
						return Ok
				}
				catch{
						return Ng "uinstall failed: $pkgName" $PSItem.Exception.Message
				}
		}
		
		[TaskResult] RunImpl($taskArgs){
				foreach($pkgName in $this.ListUninstallPackages()){
						$this.logger.Info("uninstalling: $pkgName")
						$status = $this.Uninstall($pkgName)
						if(-not $status.Ok()){
								$this.logger.Error("uninstall failed: " `
									+ $status.ErrorMessage() `
									+ " " `
									+ $status.Reason())
						}
				}

				foreach($pkgName in $this.ListInstallPackages()){
						$this.logger.Info("installing: $pkgName")
						$status = $this.Install($pkgName)
						if(-not $status.Ok()){
								$this.logger.Error("install failed: " `
									+ $status.ErrorMessage() `
									+ " " `
									+ $status.Reason())
						}
				}

				return [TaskResult]::OK
		}
}


# base class for package management by choco
class ChocoBase : PackageInstallerBase{
		hidden [string] FormatInstallCommand($pkgName){
				return "choco install `"{0}`" --yes --force" -f $pkgName
		}

		hidden [bool] FormatUninstallCommand($pkgName){
				return "choco install `"{0}`" --yes --force" -f $pkgName
		}

		hidden [bool] HasChoco(){
				try{
						Get-Command choco
						return $true
				}
				catch{
						return $false
				}
		}
		
		[TaskResult] RunImpl($taskArgs){
				$this.logger.Info("installing: chocolatey")

				if(-not $this.HasChoco()){
						# https://chocolatey.org/install#individual
						Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
				}
				
				return ([PackageInstallerBase]$this).RunImpl($taskArgs)
		}
}

class ChocoEssentials : ChocoBase{
<#
		hidden [string[]] $installPackages = @(
				"sysinternals"
				,"hxd"
				,"stirling-jp"
				,"x64dbg.portable"
				,"PeStudio"
				#,"ghidra"
		)
#>

		hidden [string[]] ListInstallPackages(){
				return $global:ChocoEssentialPackages
		}
}


# winget base
# NOTE: msstore sources requires store account.
class WingetBase : PackageInstallerBase{
		hidden [string] FormatInstallCommand($pkgName){
				return "winget install `"$pkgName`" --accept-source-agreements --accept-package-agreements --force"
		}
		hidden [string] FormatUninstallCommand($pkgName){
				return "winget uninstall `"$pkgName`" --force"
		}
		
		[bool] Runnable(){
				try{
						Get-Command winget
						$this.logger.Info("winget found")
						return $true
				}
				catch{
						$this.logger.Error("winget is not found. install winget first")
						return $false
				}
		}
}


class WingetEssentials : WingetBase{
<#
		[string[]] $installPackages = @(
				"GNU.Emacs"
				,"voidtools.Everything"
				,"Google.Chrome"
		)
		[string[]] $uninstallPackages = @(
				"Cortana"
				,"Microsoft OneDrive"
		)
#>
		hidden [string[]] ListInstallPackages(){
				return $global:WingetEssentialPackages
		}

		hidden [string[]] ListUninstallPackages(){
				return @(
						"Cortana"
						,"Microsoft OneDrive"
				)
		}
}


# install wsl
class InstallWSL : TaskBase{
		[TaskResult] RunImpl($taskArgs){
				# may be iso-2022-jp in jp env.
				$enc = [Console]::OutputEncoding 
				# wsl output is UTF-16le. temporarily change output encoding to capure wsl outputs.
				[Console]::OutputEncoding = [System.Text.Encoding]::Unicode
				
				try{
						if($this.State() -eq [TaskState]::UNEXECUTED){
								$this.logger.Info("install wsl and debian")
								try{
										wsl --install -d Debian
								}
								catch{
										# I know what I wrote.
								}
								return [TaskResult]::RERUN
						}
						
						$this.logger.Info("setting wsl default version to 1")
						wsl --set-default-version 1
						$this.logger.Info("setting wsl default distro to Debian")
						wsl --set-default Debian
						
						return [TaskResult]::OK
				}
				finally{
						[Console]::OutputEncoding = $enc
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
		hidden [int]$consumed_ = 0
		hidden [object[]]$srcArgs
		
		OptionParser([object[]]$srcArgs){
				$this.srcArgs = $srcArgs
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
				return $this.srcArgs.Count - $this.consumed_
		}

		hidden [bool] IsKey($offset){
				$pos = $this.consumed_ + $offset
				if($pos -ge $this.srcArgs.Count){
						return $false
				}
				return $this.srcArgs[$pos].StartsWith("-")
		}

		hidden [void] ParseAsFlag($offset){
				$pos = $this.consumed_ + $offset
				if($pos -ge $this.srcArgs.Count){
						return
				}

				$key = $this.srcArgs[$pos]
				$key = $key.Substring(1, $key.Length-1).ToLower()
				$this.opts_[$key] = $true
				
				$this.consumed_ += 1
		}

		hidden [void] ParseAsKeyValue($offset){
				$kpos = $this.consumed_ + $offset
				if($kpos -ge $this.srcArgs.Count){
						return
				}
				$vpos = $this.consumed_ + $offset + 1
				if($vpos -ge $this.srcArgs.Count){
						return
				}

				$key = $this.srcArgs[$kpos]
				$key = $key.Substring(1, $key.Length-1).ToLower()
				$val = $this.srcArgs[$vpos]

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

		[void] DebugPrint(){
				foreach($k in $this.opts_.Keys){
						$v = $this.opts_[$k]
						pp $k $v
				}
		}
}
Define-Global Option ([OptionParser]::new($args))


# describes how this script is being executed,
# and continuation(rerun) control.
class ExecutionContext{
		hidden [bool]$rerunRequired_ = $false
		
		ExecutionContext(){
				$this.CancelRerun()
		}

		[bool] Shell(){
				return -not $this.ScheduledTask()
		}

		[bool] ScheduledTask(){
				return $global:Option.GetOption("scheduled", $false)
		}

		hidden [string] ActionArgs(){
				return "-ep bypass -file $PSCommandPath -scheduled"
		}

		[bool] RerunRequired(){
				return $this.rerunRequired_
		}

		[void] Rerun(){
				$trigger = New-ScheduledTaskTrigger -AtLogon -User $env:UserName
				$action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument $this.ActionArgs()
				$settings = New-ScheduledTaskSettingsSet -RestartCount 5 -RestartInterval (New-TimeSpan -Minutes 1)
				Register-ScheduledTask -TaskName "setup-script" -Trigger $trigger -Action $action -Settings $settings -RunLevel Highest
				$this.rerunRequired_ = $true
		}

		[void] CancelRerun(){
				try{
						Unregister-ScheduledTask -TaskName "setup-script" -Confirm:$false
				}
				catch{
				}
		}
}
Define-Global ExecCtx ([ExecutionContext]::new())



function Main{
		if($global:ExecCtx.Shell()){
				$global:Logger.Info("$PSCommandPath executed from shell")
		}else{
				$global:Logger.Info("$PSCommandPath executed from scheduled task")
		}

		$taskExecutor = [TaskExecutor]::new()		
		function Task{
				param(
						[string]$name,
						[bool]$resetState = $false,
						[parameter(ValueFromRemainingArguments=$true)]
						[object[]]$remainings
				)

				$task = New-Object $name @remainings
				$msg = "adding task: $name"
				if($global:Option.GetOption("reset", $false)){
						$task.ResetState()
						$msg += " with state reset"
				}
				$global:Logger.Info($msg)
				
				$taskExecutor.AddTask($task)
		}

		
		if($global:Option.GetOptions("task", @()).Count -eq 0){
				Task UpdatePowershellEnv
				Task WindowsUpdate
				Task PopulateRegistryConfig
				Task DeleteDefaultShortcuts
				Task CleanTaskbar
				Task EnableRDP
				Task EnableOpenSSH
				Task InstallWSL
				Task ChocoEssentials
				Task WingetEssentials
		}else{
				$taskNames = $global:Option.GetOptions("task")
				foreach($name in $taskNames){
						Task $name
				}
		}


		if($global:Option.GetOption("ensurerunnable", $false)){
				$global:Logger.Info("testing all task is runnable or not")
				$s = $taskExecutor.IsAllTaskRunnable()
				if(-not $s.Ok()){
						$global:Logger.Error("unrunnable task found: " + $s.ErrorMessage())
						$global:Logger.Error("aborting")
						return
				}
				$global:Logger.Info("all task is runnable")
		}
		

		$bag = @{}
		$taskExecutor.Run($bag)


		if($global:ExecCtx.RerunRequired()){
				$global:Logger.Info("rerun is required. rebooting windows")
				Restart-Computer -Force
				return
		}
		

		$global:Logger.Info("all task executed")
}


function assert{
		param (
				[bool]$expr,
				[string]$msg = ""
		)
		if($expr -eq $false){
				$cs = Get-PSCallStack
				[array]::Reverse($cs)
				$tmp = "ASSERTION FAILED"
				if($msg -ne ""){
						$tmp = $tmp + ": " + $msg
				}
				Write-Host $tmp -Foreground "red"
				foreach($s in $cs){
						Write-Host $s -Foreground "red"
				}
				throw "thrown from assert"
		}
}


class TestTask : TaskBase{
		TestTask():base(){
				$this.state_.Clear()
		}
		
		[TaskResult] RunImpl($result){
				return $result
		}
}


function TestTestTask{
		try{
				$t = [TestTask]::new()
				assert ($t.Name() -eq "TestTask") "unexpected task name"
				assert $t.Runnable() "Runnable() returned false"


				$s = $t.Run([TaskResult]::ERROR)
				assert ($s.Ok() -eq $false -and $t.State() -eq [TaskState]::FAILED)

				$s = $t.Run([TaskResult]::OK)
				assert ($s.Ok() -eq $true -and $t.State() -eq [TaskState]::EXECUTED)

				$s = $t.Run([TaskResult]::RERUN)
				assert ($s.Ok() -eq $true -and $t.State() -eq [TaskState]::RERUN)
		}
		finally{
				$global:ExecCtx.CancelRerun()
		}
		
}

function TestStatus{
		$s = Ok 1
		assert ($s.Ok() -eq $true)
		assert ($s.Value() -eq 1)
		$s = Ng "a" "b"
		assert ($s.Ok() -eq $false)
		assert ($s.ErrorMessage() -eq "a")
		assert ($s.Reason() -eq "b")
}


function TestTaskStateMarker{
		$m = [TaskStateMarker]::new("test")
		$m.Clear()
		
		assert ($m.CurrentState() -eq [TaskState]::UNEXECUTED)
		$m.Mark([TaskState]::FAILED)
		assert ($m.CurrentState() -eq [TaskState]::FAILED)
}


class TestTask2 : TaskBase{
		TestTask2():base(){
				$this.state_.Clear()
		}
		
		[TaskResult] RunImpl($result){
				return $result
		}
}

function TestAll{
		TestTestTask
		TestStatus
		TestTaskStateMarker
}


Main
#TestAll
