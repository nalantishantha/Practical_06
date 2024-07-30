
import scala.io.StdIn
import scala.util.Try

object Q2 {
  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
  }

  def getStudentInfo(): (String, Int, Int, Double, Char) = {
  
    print("Enter student name:")
    val name = StdIn.readLine()
    
    print("Enter marks obtained:")
    val marks = StdIn.readInt()
    
    print("Enter total possible marks:")
    val totalMarks = StdIn.readInt()
    
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = calculateGrade(percentage)
    
    (name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println("Student Name: " + name)
    println("Marks Obtained: " + marks)
    println("Total Marks: " + totalMarks)
    println(f"Percentage:  $percentage%.2f%%")
    println("Grade: " + grade)
  }

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.trim.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0) {
      (false, Some("Marks must be a positive integer"))
    } else if (totalMarks <= 0) {
      (false, Some("Total marks must be a positive integer"))
    } else if (marks > totalMarks) {
      (false, Some("Marks obtained cannot exceed total marks"))
    } else {
      (true, None)
    }
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentInfo: (String, Int, Int, Double, Char) = null

    while (!isValid) {
      try {
        val info = getStudentInfo()
        val (name, marks, totalMarks, _, _) = info
        val (valid, errorMessage) = validateInput(name, marks, totalMarks)

        if (valid) {
          isValid = true
          studentInfo = info
        } else {
          println(s"Invalid input: ${errorMessage.getOrElse("")}")
        }
      } catch {
        case _: NumberFormatException =>
          println("Invalid input: Please enter valid numbers for marks and total marks")
      }
    }

    studentInfo
  }

  private def calculateGrade(percentage: Double): Char = {
    if (percentage >= 90) 'A'
    else if (percentage >= 75) 'B'
    else if (percentage >= 50) 'C'
    else 'D'
  }
}