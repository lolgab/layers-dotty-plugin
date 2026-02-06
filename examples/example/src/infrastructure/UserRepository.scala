package infrastructure

import application.UserService
import domain.User

class UserRepository(userService: UserService):
  def save(user: User): Unit = ()
