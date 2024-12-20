using module "c:\setup\libsetup.psm1"

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$setup = [Setup]::new("c:\setup\", "vagrant")


Task "enable-autologon-1" {
	$path = "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon"
	Set-ItemProperty $path "AutoAdminLogon" -type String -Value "1" 
	Set-ItemProperty $path "DefaultUsername" -type String -Value "vagrant"
	Set-ItemProperty $path "DefaultPassword" -type String -Value "vagrant"

	return [TaskResult]::OK
}

Task "install-lang" {
		if(Get-WinUserLanguageList | ?{$_.LanguageTag -eq "ja"}){
				$this.logger.Info("ja-JP language pack already installed")
				return [TaskResult]::OK
		}

		Install-Language -Language ja-JP -CopyToSettings
		
		Set-WinUserLanguageList -LanguageList ja-JP,en-US -Force # keyboard pref order
		Set-SystemPreferredUILanguage -Language ja-JP
		Set-WinUILanguageOverride -Language ja-JP
		Set-WinSystemLocale -SystemLocale ja-JP
		Set-Culture -CultureInfo ja-JP
		Set-WinCultureFromLanguageListOptOut -OptOut $false
		Set-WinHomeLocation -GeoId 0x7a
		Set-TimeZone -Id "Tokyo Standard Time"

		# for debug
		#.{
		#		reg query "HKLM\System\CurrentControlSet\Services\i8042prt\Parameters"
		#} | Write-Host
		
		# use JP keyboard
		reg add "HKLM\System\CurrentControlSet\Services\i8042prt\Parameters" /v "LayerDriver JPN"/t REG_SZ /d kbd106.dll /f
		reg add "HKLM\System\CurrentControlSet\Services\i8042prt\Parameters" /v "OverrideKeyboardIdentifier"/t REG_SZ /d PCAT_106KEY /f
		reg add "HKLM\System\CurrentControlSet\Services\i8042prt\Parameters" /v "OverrideKeyboardSubtype"/t REG_DWORD /d 2 /f

		return [TaskResult]::OK
}

<#
Task "enable-autologon-2" {
	$path = "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon"
	Set-ItemProperty $path "AutoAdminLogon" -type String -Value "1" 
	Set-ItemProperty $path "DefaultUsername" -type String -Value "vagrant"
	Set-ItemProperty $path "DefaultPassword" -type String -Value "vagrant"

	return [TaskResult]::REBOOT
}
#>

<#
Task "update-powershell-env" {
	$this.logger.Info("trusting PSGallery repository")
	Set-PSRepository -Name PSGallery -InstallationPolicy Trusted

	$this.logger.Info("installing PSWindowsUpdate")
	Install-Module -Name PSWindowsUpdate -Force

	return [TaskResult]::OK
}
#>

<#
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
#>

<#
Task "populate-registry-config" {
	$local:ErrorActionPreference = "Continue" # for "reg import"
	
	$this.logger.Info("populating registry configs")
	$regFiles = Get-ChildItem -Path c:\setup\resources -Filter "config.reg"
	foreach($regFile in $regFiles){
		$this.logger.Info("applying reg file: $($regFile.FullName)")
		#regedit /s $regFile.FullName
		reg import $regFile.FullName -EA SilentlyContinue
	}

	# only key
	reg add "HKLM\System\CurrentControlSet\Control\Network\NewNetworkWindowOff" /f
	
	return [TaskResult]::REBOOT
}

Task "enable-rdp" {
	$this.logger.Info("configuring firewall")

	# enable RDP
	reg add "HKLM\System\CurrentControlSet\Control\Terminal Server" /v fDenyTSConnections /t REG_DWORD /d "0" /f

	# disable NLA
	reg add "HKLM\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp" /v SecurityLayer /t REG_DWORD /d 0 /f
	reg add "HKLM\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp" /v UserAuthentication /t REG_DWORD /d 0 /f

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
	
	$sshd = Get-Service sshd -EA SilentlyContinue
	if(-not $sshd){
		for($i = 0; $i -lt 10; ++$i){
			$this.logger.Info("trying Add-WindowsCapability... $i") 
			$caps = Get-WindowsCapability -Online -Name "*openssh*"
			foreach($cap in $caps){
				if($cap.State -ne "installed"){
					$this.logger.Info("installing: $($cap.Name)")
					$rv = Add-WindowsCapability -Online -Name $cap.Name
				}
			}
			
			$sshd = Get-Service sshd -EA SilentlyContinue
			if(-not $sshd){
				foreach($cap in $caps){
					Remove-WindowsCapability -Online -Name $cap.Name -EA SilentlyContinue
				}
				continue
			}
		}
		
		# this will raise if sshd is not installed
		$sshd = Get-Service -Name sshd
	}
	
	$this.logger.Info("enabling OpenSSH")
	$sshd = Get-Service -Name sshd
	Set-Service -Name $sshd.Name -StartupType automatic
	Start-Service -Name $sshd.Name
		
	return [TaskResult]::OK
}
#>

$setup.Run()

