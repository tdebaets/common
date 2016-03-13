#!/bin/sh

# **************************************************************************
# *
# * Licensed under the Apache License, Version 2.0 (the "License");
# * you may not use this file except in compliance with the License.
# * You may obtain a copy of the License at
# *
# *     http://www.apache.org/licenses/LICENSE-2.0
# *
# * Unless required by applicable law or agreed to in writing, software
# * distributed under the License is distributed on an "AS IS" BASIS,
# * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# * See the License for the specific language governing permissions and
# * limitations under the License.
# *
# **************************************************************************
# *
# * Script to check for uncommitted changes in a Git submodule.
# * To be passed to 'git submodule foreach'
# *
# **************************************************************************

if [ $(git status --porcelain | wc -l) -gt 0 ]; then
    echo "Uncommitted local changes found in submodule '$1'"
    echo "Submodules should be considered read-only. If changes are "
    echo "required, these should be made separately in the submodule's "
    echo "repository."
    exit 1
fi
