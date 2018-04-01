import sys
from mod_pbxproj import XcodeProject

if __name__ == '__main__':
	export_path = sys.argv[1]
	script_symbols = sys.argv[2].split(';')
	print(export_path)
	print(script_symbols)

	project = XcodeProject.Load(export_path + '/Unity-iPhone.xcodeproj/project.pbxproj')
	project.add_other_ldflags('-lc++')

	frameworks = project.get_or_create_group('Frameworks')

	print('Mixpanel Post-Process: Adding CoreTelephony.framework')
	project.add_file_if_doesnt_exist('System/Library/Frameworks/CoreTelephony.framework', parent=frameworks, weak=False, tree='SDKROOT')

	# Unity 5 wrongly includes the bundle for iOS
	project.remove_file_by_path('Frameworks/Plugins/MixpanelSDK.bundle')

	print('Disabling bitcode')
	project.add_flags({'ENABLE_BITCODE':'NO'})

	project.save()