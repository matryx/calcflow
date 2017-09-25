(setq m 10)

(gentran `( ((mdo) n ,m nil nil 100 nil
			((mcond) ((mequal) k 3)
				 (($break))
				 t
				 $false)) ) nil)
