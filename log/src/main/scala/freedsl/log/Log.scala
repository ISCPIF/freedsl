/**
 * Created by Romain Reuillon on 28/10/16.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package freedsl.log

import freestyle.tagless._

@tagless trait Log {
  def print(s: String): FS[Unit]
}

case class LogInterpreter() extends Log.Handler[freedsl.dsl.Evaluated] {
  def print(s: String) = freedsl.dsl.guard(println(s))
}