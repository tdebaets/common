/****************************************************************************
 *
 * Copyright 2023 Tim De Baets
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ****************************************************************************
 *
 * Definition and implementation of CWindowProxy helper class, based on The
 * Old New Thing - How can I make a WNDPROC or DLGPROC a member of my C++
 * class?
 * https://devblogs.microsoft.com/oldnewthing/20140203-00/?p=1893
 *
 ****************************************************************************/

#pragma once

#include <Windows.h>

class IWndProcProxy
{
public:
    virtual LRESULT WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam) = 0;
};

class CWindowProxy
{
public:
    static LRESULT CALLBACK WndProcProxy(
        HWND hWnd,
        UINT uMsg,
        WPARAM wParam,
        LPARAM lParam)
    {
        LRESULT         lRes    = 0;
        IWndProcProxy  *pThis   = NULL; // our "this" pointer will go here

        if (uMsg == WM_NCCREATE)
        {
            // Recover the "this" pointer which was passed as a parameter
            // to CreateWindow(Ex).
            LPCREATESTRUCT lpcs = reinterpret_cast<LPCREATESTRUCT>(lParam);
            pThis = static_cast<IWndProcProxy *>(lpcs->lpCreateParams);
            // Put the value in a safe place for future use
            SetWindowLongPtr(hWnd, GWLP_USERDATA,
                reinterpret_cast<LONG_PTR>(pThis));
        }
        else
        {
            // Recover the "this" pointer from where our WM_NCCREATE handler
            // stashed it.
            pThis = reinterpret_cast<IWndProcProxy *>(
                GetWindowLongPtr(hWnd, GWLP_USERDATA));
        }

        if (pThis)
        {
            // Now that we have recovered our "this" pointer, let the
            // member function finish the job.
            lRes = pThis->WndProc(hWnd, uMsg, wParam, lParam);
        }
        else
        {
            // We don't know what our "this" pointer is, so just do the default
            // thing. Hopefully, we didn't need to customize the behavior yet.
            lRes = DefWindowProc(hWnd, uMsg, wParam, lParam);
        }

        if (uMsg == WM_NCDESTROY)
        {
            SetWindowLongPtr(hWnd, GWLP_USERDATA, 0);
        }

        return lRes;
    }
};
