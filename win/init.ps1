using module "c:\setup\libsetup.psm1"

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$setup = [Setup]::new("c:\setup\", $env:UserName)

Task "update-powershell-env" {
		$this.logger.Info("trusting PSGallery repository")
		Set-PSRepository -Name PSGallery -InstallationPolicy Trusted

		$this.logger.Info("installing PSWindowsUpdate")
		Install-Module -Name PSWindowsUpdate -Force

		return [TaskResult]::OK
}

Task "windows-update" {
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

Task "populate-registry-config" {
		$this.logger.Info("populating registry configs")
		$regFiles = Get-ChildItem -Path c:\setup\resources -Filter "*.reg"
		foreach($regFile in $regFiles){
				$this.logger.Info("applying reg file: $($regFile.FullName)")
				regedit /s $regFile.FullName
		}

		# only key
		reg add "HKLM\System\CurrentControlSet\Control\Network\NewNetworkWindowOff" /f

		return [TaskResult]::REBOOT
}

Task "enable-rdp" {
		$this.logger.Info("configuring firewall")

		# enable RDP
		reg add "HKLM\CurrentControlSet\Control\Terminal Server" /v fDenyTSConnections /t REG_DWORD /d 0 /f

		# disable NLA
		reg add "HKLM\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp" /v SecurityLayer /t REG_DWORD /d 0 /f
		reg add "HKLM\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp" /v UserAuthentication /t REG_DWORD /d 0 /f

		# open RDP port
		$this.logger.Info("enabling RDP firewall rules")
		foreach($rule in Get-NetFirewallRule){
				if($rule.Name -like "RemoteDesktop-UserMode-IN-*"){
						$this.logger.Info("enabling: $rule.Name")
						Enable-NetFirewallRule -Name $rule.Name
				}
		}

		return [TaskResult]::OK
}

Task "enable-ssh" {
		$this.logger.Info("installing OpenSSH")

		$rebootRequired = $false
		
		$caps = Get-WindowsCapability -Online -Name "*openssh*"
		foreach($cap in $caps){
				if($cap.State -ne "installed"){
						$this.logger.Info("installing: $($cap.Name)")
						$ret = Add-WindowsCapability -Online -Name $cap.Name
						$rebootRequired += $ret.RestartNeeded
				}
		}

		if($rebootRequired){
				return [TaskResult]::RERUN
		}

		$this.logger.Info("enabling OpenSSH")
		$sshd = Get-Service -Name sshd
		Set-Service -Name $sshd.Name -StartupType automatic
		Start-Service -Name $sshd.Name
				
		return [TaskResult]::OK
}

Task "install-winget-packages" {
		Expand-Archive -Path c:\setup\resources\Microsoft.WinGet.Client-PSModule.zip `
			-DestinationPath c:\setup\pswinget\
}


$setup.Run()

