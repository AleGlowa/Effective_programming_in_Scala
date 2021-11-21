package todo

import cats.implicits.*
import java.nio.file.{Path, Paths, Files}
import java.nio.charset.StandardCharsets
import io.circe.{Decoder, Encoder}
import io.circe.parser.*
import io.circe.syntax.*
import scala.collection.mutable
import todo.data.*

/**
 * The PersistentModel is a model that saves all data to files, meaning that
 * tasks persist between restarts.
 *
 * You should modify this file.
 */
object PersistentModel extends Model:
  import Codecs.given

  /** Path where the tasks are saved */
  val tasksPath = Paths.get("tasks.json")
  /** Path where the next id is saved */
  val idPath = Paths.get("id.json")

  /**
   * Load Tasks from a file. Return an empty task list if the file does not exist,
   * and throws an exception if decoding the file fails.
   */
  def loadTasks(): Tasks =
    if Files.exists(tasksPath) then
      load[Tasks](tasksPath)
    else
      Tasks.empty

  /**
   * Load an Id from a file. Returns Id(0) if the file does not exist, and throws
   * an exception if decoding the file fails.
   */
  def loadId(): Id =
    if Files.exists(idPath) then
      load[Id](idPath)
    else
      Id(0)

  /**
   * Load JSON-encoded data from a file.
   *
   * Given a file name, load JSON data from that file, and decode it into the
   * type A. Throws an exception on failure.
   *
   * It is not necessary to use this method. You should be able to use loadTasks
   * and loadId instead, which have a simpler interface.
   */
  def load[A](path: Path)(using decoder: Decoder[A]): A = {
    val str = Files.readString(path, StandardCharsets.UTF_8)

    // In a production system we would want to pay more attention to error
    // handling than we do here, but this is sufficient for the case study.
    decode[A](str) match {
      case Right(result) => result
      case Left(error) => throw error
    }
  }

  /**
   * Save tasks to a file. If the file already exists it is overwritten.
   */
  def saveTasks(tasks: Tasks): Unit =
    save(tasksPath, tasks)

  /**
   * Save Id to a file. If the file already exists it is overwritten.
   */
  def saveId(id: Id): Unit =
    save(idPath, id)

  /**
   * Save data to a file in JSON format.
   *
   * Given a file name and some data, saves that data to the file in JSON
   * format. If the file already exists it is overwritten.
   *
   * It is not necessary to use this method. You should be able to use saveTasks
   * and saveId instead, which have a simpler interface.
   */
  def save[A](path: Path, data: A)(using encoder: Encoder[A]): Unit =
    val json = data.asJson
    Files.writeString(path, json.spaces2, StandardCharsets.UTF_8)
    ()

  /* Hint: there are two pieces of state we need to implement the model:
   * - the tasks (idStore -> loadTasks())
   * - the next Id (nextId() -> loadId())
   * (The InMemoryModel uses the same.)
   */

  def create(task: Task): Id =
    val (id, tasks) = (loadId(), loadTasks().toMap)
    if id == Id(0) then
      Files.createFile(idPath)
      Files.createFile(tasksPath)
    saveTasks(Tasks(tasks ++ Map(id -> task)))
    saveId(id.next)
    id

  def read(id: Id): Option[Task] =
    loadTasks().tasks.find(_(0) == id).map(_(1))

  def update(id: Id)(f: Task => Task): Option[Task] =
    val tasks = loadTasks()
    tasks.toList.find(_(0) == id).map {
      case (id, task) =>
        val updatedTask = f(task)
        saveTasks(Tasks(tasks.toMap ++ Map(id -> updatedTask)))
        updatedTask
    }

  def delete(id: Id): Boolean =
    val tasks = loadTasks()
    tasks.toList.find(_(0) == id).fold(false) {
      case (id, _) =>
        saveTasks(Tasks(tasks.toMap removed id))
        true
    }

  def tasks: Tasks =
    Tasks(loadTasks().toList.sortBy(_(0)))

  def tasks(tag: Tag): Tasks =
    Tasks(loadTasks().tasks.filter(_(1).tags.contains(tag)))

  def complete(id: Id): Option[Task] =
    val tasks = loadTasks()
    tasks.toList.find(_(0) == id).map {
      case (id, task) =>
        val completedTask = task.complete
        saveTasks(Tasks(tasks.toMap ++ Map(id -> completedTask)))
        completedTask
    }

  def tags: Tags =
    Tags(loadTasks().toList.flatMap(_(1).tags).distinct)

  def clear(): Unit =
    Files.deleteIfExists(tasksPath)
    Files.deleteIfExists(idPath)
    ()
