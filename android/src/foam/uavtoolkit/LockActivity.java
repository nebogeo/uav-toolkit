// Starwisp Copyright (C) 2013 Dave Griffiths
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package foam.uavtoolkit;

import android.app.Activity;
import android.os.Bundle;
import android.content.Context;
import android.content.pm.ActivityInfo;
import android.view.WindowManager;
import android.view.WindowManager.LayoutParams;
import android.view.View;
import android.view.KeyEvent;

public class LockActivity extends foam.starwisp.StarwispActivity
{
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        m_Name = "lock";
        super.onCreate(savedInstanceState);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR_PORTRAIT);

        WindowManager.LayoutParams params = getWindow().getAttributes();
        params.flags |= LayoutParams.FLAG_TURN_SCREEN_ON;
        params.flags |= LayoutParams.FLAG_KEEP_SCREEN_ON;
        params.flags |= LayoutParams.FLAG_SHOW_WHEN_LOCKED;
        params.flags |= LayoutParams.FLAG_DISMISS_KEYGUARD;
        params.screenBrightness = 0.1F;
        getWindow().setAttributes(params);

        getWindow().getDecorView().setSystemUiVisibility(
            View.SYSTEM_UI_FLAG_LAYOUT_STABLE
            | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
            | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
            | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
            | View.SYSTEM_UI_FLAG_FULLSCREEN
            | View.SYSTEM_UI_FLAG_IMMERSIVE);

    }

    @Override
    public void onBackPressed() {
    }

    @Override
    public void onAttachedToWindow() {
        this.getWindow().setType(WindowManager.LayoutParams.TYPE_KEYGUARD_DIALOG);
        super.onAttachedToWindow();
    }

}
