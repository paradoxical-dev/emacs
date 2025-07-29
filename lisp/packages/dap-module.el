;; INFO: This module contains all the config for debugging and DAP servers

;; DYNAMIC SETUPS ;;
;; INFO: These functions are used for specific template definitions with dynamic values

(defun my/dap-gdb-provider (conf)
  "Prompt for binary when launching GDB."
  (let* ((exe (read-file-name "Path to binary: " (projectile-project-root))))
    (plist-put conf :program exe)
    (plist-put conf :dap-server-path '("gdb" "-i" "dap"))
    conf))

(defun my/dap-lldb-provider (conf)
  "Prompt for binary when launching LLDB."
  (let* ((exe (read-file-name "Path to binary: " (projectile-project-root))))
    (plist-put conf :program exe)
    (plist-put conf :dap-server-path '("lldb-dap"))
    conf))

(defun my/dap-codelldb-provider (conf)
  "Prompt for binary when launching CodeLLDB."
  (let* ((exe (read-file-name "Path to binary: " (projectile-project-root))))
    (plist-put conf :program exe)
    (plist-put conf :dap-server-path (expand-file-name ".extension/vscode/codelldb/extension/adapter/codelldb" user-emacs-directory))
    conf))

(defun my/dap-cpptools-provider (conf)
  "Prompt for binary when launching cpptools."
  (let* ((exe (read-file-name "Path to binary: " (projectile-project-root))))
    (plist-put conf :program (expand-file-name exe))
    (plist-put conf :dap-server-path (list (expand-file-name ".extension/vscode/cpptools/extension/debugAdapters/bin/OpenDebugAD7" user-emacs-directory)))
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
	 :cwd "${workspaceFolder}"
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
  (dap-register-debug-provider "lldb-dynamic" #'my/dap-lldb-provider)
  (dap-register-debug-template
   "LLDB: Prompt Binary"
   (list :type "lldb-dynamic"
	 :request "launch"
	 :name "LLDB: Prompt Binary"
	 :stopAtEntry :json-false
	 :externalConsole :json-false
	 :cwd "${workspaceFolder}"
	 :args []))

  (require 'dap-codelldb)
  ;; INFO: This needs to be manually installed from the release page.
  ;; Head to the release page and use a command like:
  ;; `wget https://github.com/vadimcn/vscode-lldb/releases/download/v1.11.5/codelldb-linux-x64.vsix`
  (setq dap-codelldb-debug-program (expand-file-name ".extension/vscode/codelldb/extension/adapter/codelldb" user-emacs-directory))
  (dap-register-debug-provider "codelldb-dynamic" #'my/dap-lldb-provider)
  (dap-register-debug-template
   "CodeLLDB: Prompt Binary"
   (list :type "codelldb-dynamic"
	 :request "launch"
	 :name "CodeLLDB: Prompt Binary"
	 :externalConsole :json-false
	 :cwd "${workspaceFolder}"
	 :env '(("RUST_BACKTRACE" . "full"))
	 :args []))

  ;; cpptools
  ;; INFO: Needs the one-time command `dap-cpptools-setup` before useable
  (require 'dap-cpptools)
  (dap-register-debug-provider "cppdbg" #'my/dap-cpptools-provider)
  (dap-register-debug-template
   "cpptools: Prompt Binary"
   (list :type "cppdbg"
	 :request "launch"
	 :name "cpptools: Prompt Binary"
	 :external-console :json-false
	 :cwd "${workspaceFolder}"
	 :MIMode "gdb"
	 :args []))

  ;; firefox
  ;; Needs the one-time command `dap-firefox-setup` before useable
  (require 'dap-firefox)

  :bind
  (:map evil-normal-state-map ("<leader>db" . dap-breakpoint-toggle))
  (:map evil-normal-state-map ("<leader>dc" . dap-continue))
  (:map evil-normal-state-map ("<leader>di" . dap-step-in))
  (:map evil-normal-state-map ("<leader>do" . dap-step-out))
  (:map evil-normal-state-map ("<leader>dn" . dap-next))
  (:map evil-normal-state-map ("<leader>de" . dap-eval))
  (:map evil-normal-state-map ("<leader>dE" . dap-eval-region))
  (:map evil-normal-state-map ("<leader>dt" . dap-ui-repl))
  (:map evil-normal-state-map ("<leader>dr" . dap-debug-restart))
  (:map evil-normal-state-map ("<leader>dx" . dap-disconnect))
  (:map evil-normal-state-map ("<leader>dd" . dap-debug)))

(provide 'dap-module)
