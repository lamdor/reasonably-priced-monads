package object eg {
  trait Monad[M[_]] {
    def pure[A](a: A): M[A]
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    def apply[M[_]: Monad] = implicitly[Monad[M]]
  }

  sealed trait ~>[F[_], G[_]] { self => 
    def apply[A](fa: F[A]): G[A]

    def or[H[_]](f: H ~> G) =
      new (({type l[x] = Coproduct[F,H,x]})#l ~> G) {
        def apply[A](c: Coproduct[F,H,A]): G[A] = c.run match {
          case Left(fa)  => self.apply(fa)
          case Right(ha) => f.apply(ha)
        }
      }
  }

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      this match {
        case Return(a) => f(a)
        case Bind(i,k) => Bind(i, k andThen (_ flatMap f))
      }

    def map[B](f: A => B): Free[F,B] =
      flatMap(a => Return(f(a)))

    def foldMap[G[_]: Monad](f: F ~> G): G[A] =
      this match {
        case Return(a) => Monad[G].pure(a)
        case Bind(fx, g) =>
          Monad[G].flatMap(f(fx)) { a =>
            g(a).foldMap(f)
          }
      }
  }
  case class Return[F[_], A](a: A) extends Free[F,A]
  case class Bind[F[_], I, A](
    i: F[I],
    k: I => Free[F,A]
  ) extends Free[F,A]

  object Free {
    implicit def lift[F[_], A](fa: F[A]): Free[F,A] =
      Bind(fa, (a: A) => Return(a))  
  }
  
  sealed trait Interact[A]

  case class Ask(prompt: String)
      extends Interact[String]

  case class Tell(msg: String)
      extends Interact[Unit]

  import Free._
  val prg: Free[Interact, Unit] =
    for {
      first <- Ask("What is your first name?")
      last  <- Ask("What is your last name?")
      _     <- Tell(s"Hello, $first $last")
    } yield ()


  type Id[A] = A

  implicit val idMonad = new Monad[Id] {
    def pure[A](a: A): Id[A] = a
    def flatMap[A,B](a: Id[A])(f: A => Id[B]) = f(a)
  }

  object Console extends (Interact ~> Id) {
    def apply[A](i: Interact[A]): Id[A] = i match {
      case Ask(prompt) =>
        println(prompt)
        io.StdIn.readLine
      case Tell(msg) =>
        println(msg)
    }
  }

  type Tester[A] = Map[String,String] => (List[String], A)
  object Test extends (Interact ~> Tester) {
    def apply[A](i: Interact[A]): Tester[A] = i match {
      case Ask(prompt) =>
        m => (Nil, m(prompt))
      case Tell(msg) =>
        m => (List(msg), ())
    }
  }
  implicit val testerMonad = new Monad[Tester] {
    def pure[A](a: A) = m => (Nil, a)
    def flatMap[A,B](t: Tester[A])(f: A => Tester[B]) =
      m => {
        val (o1, a) = t(m)
        val (o2, b) = f(a)(m)
        (o1 ++ o2, b)
      }
  }

  val interactMap = Map(
    "What is your first name?" -> "Luke",
    "What is your last name?" -> "Amdor"
  )

  lazy val prgTest = prg.foldMap(Test).apply(interactMap)

  type User = String
  type UserId = String
  type Password = String

  sealed trait Permission
  object KnowTheSecret extends Permission

  sealed trait Auth[A]
  case class Login(u: UserId, p: Password) extends Auth[User]
  case class HasPermission(u: User, p: Permission) extends Auth[Boolean]


  case class Coproduct[F[_], G[_], A](
    run: Either[F[A], G[A]]
  )

  type App[A] = Coproduct[Interact, Auth, A]

  sealed trait Inject[F[_], G[_]] {
    def inj[A](sub: F[A]): G[A]
  }
  object Inject {
    implicit def refl[F[_]]: Inject[F,F] = new Inject[F,F] {
      def inj[A](fa: F[A]) = fa
    }
    implicit def left[F[_], G[_]]: Inject[F, ({type l[x] = Coproduct[F,G,x]})#l] =
      new Inject[F, ({type l[x] = Coproduct[F,G,x]})#l] {
        def inj[A](fa: F[A]) = Coproduct(Left(fa))
      }
    implicit def right[F[_], G[_], H[_]](implicit I: Inject[F,G]) =
      new Inject[F, ({type l[x] = Coproduct[H,G,x]})#l] {
        def inj[A](fa: F[A]) = Coproduct(Right(I.inj(fa)))
      }

    def lift[F[_], G[_], A](fa: F[A])(implicit I: Inject[F,G]): Free[G,A] =
      Bind(I.inj(fa), Return(_: A))
  }

  class Interacts[F[_]](implicit I: Inject[Interact,F]) {
    import Inject._
    def tell(msg: String): Free[F,Unit] = lift(Tell(msg))
    def ask(prompt: String): Free[F,String] = lift(Ask(prompt))
  }
  object Interacts {
    implicit def instance[F[_]](implicit I: Inject[Interact,F]) =
      new Interacts
  }

  class Auths[F[_]](implicit I: Inject[Auth, F]) {
    import Inject._
    def login(uid: UserId, password: Password): Free[F,User] =
      lift(Login(uid, password))
    def hasPermission(user: User, permission: Permission): Free[F,Boolean] =
      lift(HasPermission(user, permission))
  }
  object Auths {
    implicit def instance[F[_]](implicit I: Inject[Auth,F]) =
      new Auths
  }

  def prg2[F[_]](implicit I: Interacts[F],
                          A: Auths[F]): Free[F, Unit] = {
    import I._, A._

    for {
      uid <- ask("What is your user ID?")
      pwd <- ask("Password please.")
      u   <- login(uid, pwd)
      b   <- hasPermission(u, KnowTheSecret)
      _   <- if (b) tell("UUDDLRLRBA")
             else   tell("Go away.")
    } yield ()

  }

  object TestAuth extends (Auth ~> Id) {
    def apply[A](fa: Auth[A]) = fa match {
      case Login("luke.amdor", "password")      => "luke"
      case Login(u, _)                          => u
      case HasPermission("luke", KnowTheSecret) => true
      case HasPermission(_, _)                  => false
    }
  }

  val app: Free[App, Unit] = prg2[App]

  def runApp: Unit = app.foldMap(Console or TestAuth)
}

