# audit

First in terminal:

    `stack exec -- audit-exe --audit audit`

This generates a dot file and is considered the "original" dot file for the repo. Now you can make changes (add/remove deps change versions)

Run `stack build` then `stack exec -- audit-exe --audit audit` again. The program will print out the dependency changes.
