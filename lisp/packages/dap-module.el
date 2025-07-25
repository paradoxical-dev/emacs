;; INFO: This module contains all the config for debugging and DAP servers

;; DYNAMIC SETUPS ;;
;; INFO: These functions are used for specific template definitions with dynamic values

(defun my/dap-gdb-provider (conf)
  "Prompt for binary when launching GDB."
  (let* ((exe (read-file-name "Path to binary: " (projectile-project-root))))
    (plist-put conf :program exe)
    (plist-put conf :cwd (file-name-directory exe))
    (plist-put conf :dap-server-path '("gdb" "-i" "dap"))
    conf))

;; IMPLEMENTATION ;;
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)

  ;; LANGAUGES ;;
  ;; python
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  ;; gdb
  (require 'dap-gdb)
  (dap-register-debug-provider "gdb-dynamic" #'my/dap-gdb-provider)
  (dap-register-debug-template
   "GDB: Prompt Binary"
   (list :type "gdb-dynamic"
	 :request "launch"
	 :name "GDB: Prompt Binary"
	 :stopAtEntry :json-false
	 :externalConsole :json-false
	 :MIMode "gdb"
	 :setupCommands [
			 (list :description "Enable pretty-printing for gdb"
                               :text "-enable-pretty-printing"
                               :ignoreFailures t)
			 (list :description "Set Disassembly Flavor to Intel"
                               :text "-gdb-set disassembly-flavor intel"
                               :ignoreFailures t)]))

  ;; lldb
  (require 'dap-lldb)
  (setq dap-lldb-debug-program (list (expand-file-name "~/.nix-profile/bin/lldb-dap")))
  (dap-register-debug-template
   "test 1"
   (list :type "lldb-vscode"
	 :request "launch"
	 :name "test 1"
	 :stopAtEntry t
	 :externalConsole :json-false
	 :program "/home/gitmoney/debug-tests/debug-rust/target/debug/debug-rust"
	 :cwd "/home/gitmoney/debug-tests/debug-rust"
	 :args []))

  (require 'dap-codelldb)
  ;; INFO: This needs to be installed from the release page. Use this command:
  ;; `wget https://github.com/vadimcn/vscode-lldb/releases/download/v1.11.5/codelldb-linux-x64.vsix`
  (setq dap-codelldb-debug-program (expand-file-name "~/.local/share/codelldb/extension/adapter/codelldb"))
  (dap-register-debug-template
   "test 2"
   (list :type "lldb"
	 :request "launch"
	 :name "test 2"
	 :externalConsole :json-false
	 :program "/home/gitmoney/debug-tests/debug-rust/target/debug/debug-rust"
	 :cwd "/home/gitmoney/debug-tests/debug-rust"
	 :env '(("RUST_BACKTRACE" . "full"))
	 :args []))

  ;; cpptools
  ;; INFO: Needs the one-time command `dap-cpptools-setup` before useable
  (require 'dap-cpptools)

  :bind
  (:map evil-normal-state-map ("<leader>db" . dap-breakpoint-toggle))
  (:map evil-normal-state-map ("<leader>dc" . dap-continue))
  (:map evil-normal-state-map ("<leader>di" . dap-step-in))
  (:map evil-normal-state-map ("<leader>dr" . dap-debug-restart))
  (:map evil-normal-state-map ("<leader>dx" . dap-disconnect))
  (:map evil-normal-state-map ("<leader>dd" . dap-debug)))

(provide 'dap-module)
