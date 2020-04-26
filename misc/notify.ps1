Import-Module BurntToast
New-BurntToastNotification -Text "$($args[0])",
                                 "$($args[1])"