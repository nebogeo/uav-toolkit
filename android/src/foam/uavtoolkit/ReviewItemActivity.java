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

public class ReviewItemActivity extends foam.starwisp.StarwispActivity
{
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        m_Name = "review-item";
        super.onCreate(savedInstanceState);
    }
}
