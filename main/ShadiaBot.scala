import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random
import scala.util.control.Breaks._
import scala.io.StdIn._
import scala.util.control.Breaks._
import java.io.{FileWriter, PrintWriter}
import py4j.GatewayServer
import java.io.{File, FileWriter, BufferedWriter, PrintWriter}
import scala.collection.mutable.ListBuffer
import java.io.File
import scala.io.Source
import java.util.{List => JList, ArrayList}

case class SymptomMatch(symptom: String, disease: String, weight: Double)
case class DiseaseInfo(
  name: String,
  description: String,
  severity: String,
  urgency: String,
  precautions: String
)
// Main HealthBot Object
object HealthBot {
  private val responseBuffer = new StringBuilder
  private val responseLock = new AnyRef

  var username = "User"
  def receiveUsername(user: String): Unit = {
    username = user
    println(s"✅ Received username: $username")
  }

  var msg = " "
  var knownDiseases: Set[String] = Set()

  var diseaseInfoMap: Map[String, DiseaseInfo] = Map()
  var symptomDiseaseList: List[SymptomMatch] = List()

  var synonymToCanonical: Map[String, String] = Map()

  val yesSynonyms = Set(
    "yes", "y", "yeah", "yep", "yup", "sure",
    "ok", "okay", "absolutely", "definitely", "agree",
    "true", "1", "affirmative", "roger", "aye", "uh-huh"
  )
  val noSynonyms = Set(
    "no", "n", "nope", "nah", "negative",
    "false", "0", "disagree", "not", "never",
    "nay", "uh-uh", "deny", "refuse"
  )
  val idkSynonyms = Set(
    "idk", "i don't know", "dunno", "unsure",
    "uncertain", "maybe", "perhaps", "not sure",
    "no idea", "who knows", "your guess is as good as mine",
    "beats me", "search me", "don't ask me"
  )
  val exitSynonyms = Set(
    "exit", "quit", "end", "stop", "bye", "goodbye",
    "i'm done", "that's all", "nothing", "i don't want anything",
    "no thanks", "not interested", "end chat", "close", "leave"
  )
  val negationWords = Set("no", "not", "don't", "doesn't", "didn't", "without", "none", "never", "lack", "except", "but not")
  

  //Sending response to gui
  def getAndClearResponses(): String = responseLock.synchronized {
    val result = responseBuffer.toString
    responseBuffer.clear()
    result
  }
  
  def appendResponse(message: String): Unit = responseLock.synchronized {
    responseBuffer.append(message).append("\n")
  }

  @volatile private var guiInput: Option[String] = None
  private val inputLock = new Object
  
  //gets input from gui
  def provideInputFromGUI(input: String): Unit = inputLock.synchronized {
    guiInput = Some(input)
    inputLock.notify()
  }

  def getNextInput(timeoutMillis: Long = 30000): Option[String] = inputLock.synchronized {
    if (guiInput.isEmpty) inputLock.wait(timeoutMillis)
    val input = guiInput
    guiInput = None
    input
  }
  
  //fuzzy words checking (typos)
  def levenshtein(s1: String, s2: String): Int = {
    val lenStr1 = s1.length
    val lenStr2 = s2.length
    val dist = Array.ofDim[Int](lenStr1 + 1, lenStr2 + 1)
    for (i <- 0 to lenStr1) dist(i)(0) = i
    for (j <- 0 to lenStr2) dist(0)(j) = j
    for {
      i <- 1 to lenStr1
      j <- 1 to lenStr2
    } {
      val cost = if (s1(i - 1) == s2(j - 1)) 0 else 1
      dist(i)(j) = List(
        dist(i - 1)(j) + 1,
        dist(i)(j - 1) + 1,
        dist(i - 1)(j - 1) + cost
      ).min
    }
    dist(lenStr1)(lenStr2)
  }
  
  
  def findClosestSymptom(input: String, symptomSet: Set[String], maxDistance: Int = 2): Option[String] = {
    val candidates = symptomSet.filter(sym => levenshtein(input, sym) <= maxDistance)
    if (candidates.isEmpty) None
    else Some(candidates.minBy(sym => levenshtein(input, sym)))
  }
  
  //loads the known symptoms of known diseases from csv file
  def loadSymptomsCSV(path: String): Unit = {
    val source = Source.fromFile(path)
    val lines = source.getLines().drop(1)

    symptomDiseaseList = lines.flatMap { line =>
      val cols = line.split(",").map(_.trim.toLowerCase)
      if (cols.length >= 3) {
        try {
          Some(SymptomMatch(cols(0).replace("_", " "), cols(1), cols(2).toDouble))
        } catch {
          case _: NumberFormatException =>
            println(s"Skipping line due to number format: $line")
            None
        }
      } else {
        println(s"Skipping malformed line (not enough columns): $line")
        None
      }
    }.toList

    source.close()
  }
  
  // loads all known diseases info 
  def loadDiseaseInfoCSV(path: String): Unit = {
    val source = Source.fromFile(path)
    val lines = source.getLines().drop(1)

    diseaseInfoMap = lines.flatMap { line =>
      val cols = line
        .split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)") // handles CSV with quoted commas
        .map(_.stripPrefix("\"").stripSuffix("\"").trim)

      if (cols.length >= 1) {
        val name = if (cols.length > 0) cols(0) else "Unknown Disease"
        val description = if (cols.length > 1 && cols(1).nonEmpty) cols(1) else "No description available."
        val severity = if (cols.length > 2 && cols(2).nonEmpty) cols(2) else "Unknown"
        val urgency = if (cols.length > 3 && cols(3).nonEmpty) cols(3) else "Unknown"
        val precautions = if (cols.length > 4) cols.drop(4).mkString("; ").trim else "No precautions available."

        Some(name.toLowerCase -> DiseaseInfo(name, description, severity, urgency, precautions))
      } else {
        println(s"Skipping malformed disease info line: $line")
        None
      }
    }.toMap
    source.close()
  }
  
  
  def loadSynonymsCSV(path: String): Unit = {
    val source = Source.fromFile(path)
    val lines = source.getLines().drop(1)
    var mapBuilder = Map[String, String]()
    for (line <- lines) {
      val cols = line.split(",").map(_.trim.toLowerCase)
      val canonical = cols(0).replace("_", " ")
      for (syn <- cols) {
        val cleaned = syn.replace("_", " ")
        mapBuilder += cleaned -> canonical
      }
    }
    synonymToCanonical = mapBuilder
    source.close()
  }
  

  def extractSymptomsFromInput(input: String): (Set[String], Set[String]) = {
    val normalizedInput = input.toLowerCase.trim
    val words = normalizedInput.split("\\s+").toList
    var recognizedSymptoms = Set[String]()
    var negatedSymptoms = Set[String]()
    var skipUntil = -1
    val maxN = 5 // max phrase length to check

    for (start <- words.indices) {
      if (start <= skipUntil) {
        // Skip tokens part of already matched phrase
      } else {
        var foundMatch: Option[(String, Int)] = None

        for (n <- maxN to 1 by -1 if foundMatch.isEmpty) {
          if (start + n <= words.length) {
            val phrase = words.slice(start, start + n).mkString(" ")
            synonymToCanonical.get(phrase) match {
              case Some(canonical) =>
                foundMatch = Some((canonical, start + n - 1))
              case None => ()
            }
          }
        }
        foundMatch match {
          case Some((canonical, endIdx)) =>
            val contextWindowStart = math.max(0, start - 2)
            val context = words.slice(contextWindowStart, start).toSet
            val isNegated = context.exists(word => negationWords.contains(word))
            if (isNegated) negatedSymptoms += canonical
            else recognizedSymptoms += canonical
            skipUntil = endIdx
          case None => ()
        }
      }
    }
    (recognizedSymptoms, negatedSymptoms)
  }
  //sets matching score of matched diseases
  def scoreDiseases(symptoms: Set[String]): List[(String, Double)] = {
    symptomDiseaseList
      .filter(s => symptoms.contains(s.symptom))
      .groupBy(_.disease)
      .view.mapValues(entries => entries.map(_.weight).sum)
      .toList.sortBy(-_._2)
  }
  // gets all the other symptoms of the matched diseases 
  def getAdditionalSymptoms(candidateDiseases: Set[String], knownSymptoms: Set[String]): Set[String] = {
    symptomDiseaseList
      .filter(s => candidateDiseases.contains(s.disease))
      .map(_.symptom)
      .toSet -- knownSymptoms
  }

  def printDiagnosis(scores: List[(String, Double)]): Unit = {
    if (scores.isEmpty) {
      msg = "❌ Sorry, I couldn't find any likely conditions based on your symptoms."
      appendResponse(msg)
      logChat("Bot", msg)
    } else {
      msg = "🩺 Based on your symptoms, here are some possible conditions:\n"
      appendResponse(msg)
      logChat("Bot", msg)

      scores.take(3).foreach { case (disease, score) =>
        val info = diseaseInfoMap.getOrElse(
          disease.toLowerCase,
          DiseaseInfo(disease, "No description available.", "Unknown", "Unknown", "No precautions available.")
        )

        msg = s"🔹 ${info.name}"
        val msg1 = f"   Match Confidence: ${score}%.2f"
        val msg2 = s"   Description: ${info.description}"

        appendResponse(msg)
        appendResponse(msg1)
        appendResponse(msg2)
        logChat("Bot", s"$msg")
        logChat("Bot", s"$msg1")        
        logChat("Bot", s"$msg2")

        info.urgency.toLowerCase match {
          case "high"    => msg = "   ⚠️ Urgency: This condition may require immediate medical attention. Please consult a doctor."
          case "medium"  => msg = "   ⏳ Suggested: Schedule a visit with your healthcare provider soon."
          case "low"     => msg = "   ℹ️ Low urgency: Keep monitoring, and make lifestyle improvements."
          case _         => msg = "   ⚠️ Urgency: Not specified. Please monitor your symptoms carefully."
        }
        appendResponse(msg)
        logChat("Bot", msg)

        info.severity.toLowerCase match {
          case "high"    => msg = "   ❗ Severity: This condition is serious and may significantly affect your health."
          case "medium"  => msg = "   ❕ Severity: This condition is manageable but shouldn't be ignored."
          case "low"     => msg = "   ✅ Severity: This condition is generally mild."
          case _         => msg = "   ❔ Severity: Not specified."
        }
        appendResponse(msg)
        logChat("Bot", msg)

        msg = "   📋 Precautions you can follow:"
        appendResponse(msg)
        logChat("Bot", msg)
        val bulletPoints = info.precautions
          .split("[;•·●]")
          .map(_.trim)
          .filter(_.nonEmpty)
        bulletPoints.foreach(p => {
          appendResponse(s"     - $p")
          logChat("Bot", s"     - $p")
        })
      }

      appendResponse("⚠️ Reminder: This is not a medical diagnosis. Always consult a qualified professional.")
    }
  }
  // diagnoses the symptoms that the user provided
  def diagnoseWithFollowUpBatched(
  initialSymptoms: Option[Set[String]] = None
): Unit = {
  var collectedSymptoms: Set[String] = initialSymptoms.getOrElse(Set())
  var negatedSymptoms: Set[String] = Set()
  val knownSymptoms = symptomDiseaseList.map(_.symptom).toSet
  var continueLoop = true
  var followUpCount = 0
  val maxFollowUps = 3 // Maximum number of follow-up question batches

  // Initial symptom collection
  while (continueLoop && collectedSymptoms.size < 2) {
    appendResponse(s"Recognized symptoms so far: ${collectedSymptoms.mkString(", ")}")
    logChat("Bot", s"Recognized symptoms so far: ${collectedSymptoms.mkString(", ")}")
    appendResponse("Please enter another symptom that you may have experienced.")
    logChat("Bot", "Please enter another symptom that you may have experienced.")
    

    val inputRaw = getNextInput().getOrElse("")
    val input = inputRaw.toLowerCase.trim
    if (input.isEmpty) {
      appendResponse("No input received. Please enter your symptoms or type 'exit' to quit.")
      logChat("Bot", "No input received. Please enter your symptoms or type 'exit' to quit.")
      continueLoop = false
      return
    }

    logChat(username, input)

    if (exitSynonyms.contains(input)) {
      appendResponse("Exiting diagnosis.")
      logChat("Bot", "Exiting diagnosis.")
      return
    }

    val (matchedRecognized, matchedNegated) = extractSymptomsFromInput(input)
    collectedSymptoms ++= matchedRecognized
    negatedSymptoms ++= matchedNegated
    collectedSymptoms --= negatedSymptoms

    if (matchedRecognized.isEmpty && matchedNegated.isEmpty) {
      val tokens = input.split("[ ,]+").map(_.trim).filter(_.nonEmpty)
      val fuzzyMatches = tokens.flatMap(tok => findClosestSymptom(tok, knownSymptoms)).toSet
      if (fuzzyMatches.isEmpty) {
        appendResponse("I couldn't recognize any symptoms in your input, please try again.")
        logChat("Bot", "I couldn't recognize any symptoms in your input, please try again.")
      } else {
        collectedSymptoms ++= fuzzyMatches
        appendResponse(s"Recognized symptoms so far: ${collectedSymptoms.mkString(", ")}")
        logChat("Bot", s"Recognized symptoms so far: ${collectedSymptoms.mkString(", ")}")
      }
    } else {
      appendResponse(s"Recognized symptoms so far: ${collectedSymptoms.mkString(", ")}")
      logChat("Bot", s"Recognized symptoms so far: ${collectedSymptoms.mkString(", ")}")
    }

    if (collectedSymptoms.size < 2) {
      appendResponse("Please enter at least 2 symptoms to proceed.")
      logChat("Bot", "Please enter at least 2 symptoms to proceed.")
    }
  }

  // Follow-up questions (limited to maxFollowUps batches)
  if (collectedSymptoms.size >= 2 && continueLoop) {
    val candidateDiseases = scoreDiseases(collectedSymptoms).take(5).map(_._1).toSet
    val additionalSymptoms = getAdditionalSymptoms(candidateDiseases, collectedSymptoms).toList.sorted
    val batches = additionalSymptoms.grouped(8).toList.take(maxFollowUps) // Limit to maxFollowUps batches

    for (batch <- batches if continueLoop && followUpCount < maxFollowUps) {
      appendResponse("\nDo you also have any of these symptoms? Type those you have, or 'done' if none:")
      logChat("Bot", "Do you also have any of these symptoms? Type those you have, or 'done' if none:")
      batch.foreach(s => {
        appendResponse(s"- $s")
        logChat("Bot", s"- $s")
      })

      val answerRaw = getNextInput().getOrElse("")
      val answer = answerRaw.toLowerCase.trim
      logChat(username, answer)

      if (answer == "done" || exitSynonyms.contains(answer)) {
        continueLoop = false
      } else {
        val (answerRecognized, answerNegated) = extractSymptomsFromInput(answer)
        collectedSymptoms ++= answerRecognized
        negatedSymptoms ++= answerNegated
        collectedSymptoms --= negatedSymptoms

        if (answerRecognized.isEmpty && answerNegated.isEmpty) {
          val tokens = answer.split("[ ,]+").map(_.trim).filter(_.nonEmpty)
          collectedSymptoms ++= tokens.flatMap(tok => findClosestSymptom(tok, knownSymptoms))
        }
      }
      followUpCount += 1
    }

    // After follow-ups or if user said 'done', show diagnosis
    val finalScores = scoreDiseases(collectedSymptoms)
    updateSymptoms(username, collectedSymptoms.toList)
    printDiagnosis(finalScores)
  }
}
  
  def respondWithDiseaseInfo(diseaseOpt: Option[Set[String]]): Unit = {
    diseaseOpt match {
      case Some(diseases) =>
        diseases.foreach { disease =>
          val info = diseaseInfoMap.getOrElse(disease.toLowerCase,
            DiseaseInfo(disease, "No description available.", "Unknown", "Unknown", "No precautions available.")
          )
          appendResponse(s"\n🦠 Disease Information for: ${info.name}")
          logChat("Bot",s"🦠 Disease Information for: ${info.name}")
          appendResponse(s"📄 Description: ${info.description}")
          logChat("Bot",s"📄 Description: ${info.description}")
          appendResponse(s"🚦 Severity: ${info.severity}")
          logChat("Bot",s"🚦 Severity: ${info.severity}")
          appendResponse(s"⏱️ Urgency: ${info.urgency}")
          logChat("Bot",s"⏱️ Urgency: ${info.urgency}")
          appendResponse("🩺 Precautions:")
          info.precautions.split("[;•·●]").map(_.trim).filter(_.nonEmpty).foreach(p => {appendResponse(s"  - $p")
                                                                                        logChat("Bot", s"- $p")})
        }
      case None =>
        appendResponse("❌ Sorry, I couldn't find info for that disease.")
    }
  }

  def loadKnownDiseases(path: String): Set[String] = {
    val source = Source.fromFile(path)
    val lines = source.getLines().drop(1) // skip header
    val diseases = lines.map { line =>
      val cols = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")  // handle commas inside quotes
      if (cols.nonEmpty) cols(0).trim.toLowerCase else ""
    }.filter(_.nonEmpty).toSet
    source.close()
    diseases
  }
 

  //handles any input the user sends 
  def HandleUserInput(input: String): Unit = {
    val wordList = input.split(" ").toList.map(_.toLowerCase)
    val lowerInput = input.toLowerCase.trim
    val knownSymptoms = symptomDiseaseList.map(_.symptom).toSet
    val knownDiseases = diseaseInfoMap.keySet  
     
    val quizKeywords = Set(
      "quiz", "test", "assessment", "evaluation", "check", "survey", "questionnaire"
    )
    val analyticsKeywords = Set(
    "analytics", "report", "summary", "stats", "statistics", "progress", "overview", "history", "record"
  )

   val hiKeywords = Set(
  "hi", "hey", "heyy", "hello", "howdy", "greetings", "hola", "hi there", "hey there", "good morning", "good afternoon", "good evening","bonjour"
)
    val byeKeywords = Set(
   "exit" ,"bye", "goodbye", "see you", "farewell", "take care", "later", "catch you", "ciao","au revoir","salamo 3aleko"
)
   val thankYouKeywords = Set(
  "thank you", "thanks", "thx", "ty", "much appreciated", "grateful", "i appreciate it", "many thanks", "thanks a lot", "thanks so much", "thank u","merci","shokran"
)
   val loveKeywords = Set(
  "i love you", "love you", "love u", "luv you", "i adore you", "i’m in love with you", "u mean a lot to me", "you’re the best", "you are the best", "i care about you","luv u","je t'aime","bahebek"
)
   val missYouKeywords = Set(
  "i miss you", "miss you", "i missed you", "missed you", "i’ve been missing you", "i really miss you", "miss u", "i’ve missed you","wahshany","wahshteeny","wahashtiny"
)

    val matchedDisease = knownDiseases.find(disease =>
      lowerInput.contains(disease.toLowerCase) || wordList.contains(disease.toLowerCase)
    )
    val byeExist = wordList.exists(word => byeKeywords.contains(word))
    val hiExist = wordList.exists(word => hiKeywords.contains(word))
    val quizExist = wordList.exists(word => quizKeywords.contains(word))
    val symptomsExist = wordList.exists(word => knownSymptoms.contains(word))
    val analyticsExist = wordList.exists(word => analyticsKeywords.contains(word))
    val loveExist = loveKeywords.exists(keyword => lowerInput.contains(keyword))
    val missYouExist = missYouKeywords.exists(keyword => lowerInput.contains(keyword))
    val thankYouExist = thankYouKeywords.exists(keyword => lowerInput.contains(keyword))
    matchedDisease match {
        case Some(disease) =>
          logChat(username, input)
          respondWithDiseaseInfo(Some(Set(disease)))
          return // Stop further processing
        case None => // Continue
      }
    if (input.contains("Basma")|| input.contains("basma")) {
      logChat(username,input)
      appendResponse("Special thanks to Dr. Basma for her guidance and support — this project wouldn’t have come to life without her insights and encouragement! 🌟\n We survived the bugs thanks to her hugs ❤")
     
       logChat("Bot", "Special thanks to Dr. Basma for her guidance and support — this project wouldn’t have come to life without her insights and encouragement! 🌟\n We survived the bugs thanks to her hugs ❤")
    }
    
    else if(input.toLowerCase().contains("slay")){
        msg="Mathazarsh enta el so slay!"
        logChat(username,input)
        appendResponse(msg)
        logChat("Bot", msg)
    }
    else if (hiExist) {
      initializeChatFile()
      logChat(username, input)
      //logChat("Bot", "New Chat")

      val feeling = GreetUser(username)  // greeting + get feeling

      val followUp = "How can I help you today?"
      appendResponse(followUp)
      logChat("Bot", followUp)
      
      // Now you can continue your logic, e.g. parse user feeling or wait for next input
}

    else if (byeExist){

       msg="Goodbye! Feel free to come back if you have more questions."
       appendResponse(msg)
       logChat(username, input)
       logChat("Bot",msg)
    }

    else if (thankYouExist) {
        logChat(username, input)
        val msg = "You're very welcome! 😊 I'm here whenever you need support."
        appendResponse(msg)
        logChat("Bot", msg)
      }

      else if (loveExist) {
        logChat(username, input)
        val msg = "Aww! ❤ That means a lot. I'm always here for you!"
        appendResponse(msg)
        logChat("Bot", msg)
      }

      else if (missYouExist) {
        logChat(username, input)
        val msg = "I missed you too! 😊 It’s always good to hear from you."
        appendResponse(msg)
        logChat("Bot", msg)
}
     else {
      // If no disease detected, proceed with the usual branching for symptoms, quizzes, analytics
      (quizExist, symptomsExist, analyticsExist) match {
        case (true, true, true) =>
          logChat(username, input)
          parseQuizInput(wordList)
          diagnoseWithFollowUpBatched(Some(wordList.toSet.intersect(knownSymptoms)))
          parseAnalyticsInput(wordList)
  
        case (true, true, false) =>
          logChat(username, input)
          parseQuizInput(wordList)
          diagnoseWithFollowUpBatched(Some(wordList.toSet.intersect(knownSymptoms)))
  
        case (false, true, true) =>
          logChat(username, input)
          diagnoseWithFollowUpBatched(Some(wordList.toSet.intersect(knownSymptoms)))
          parseAnalyticsInput(wordList)
  
        case (true, false, true) =>
          logChat(username, input)
          parseQuizInput(wordList)
          parseAnalyticsInput(wordList)
  
        case (true, false, false) =>
          logChat(username, input)
          parseQuizInput(wordList)
  
        case (false, true, false) =>
          logChat(username, input)
          diagnoseWithFollowUpBatched(Some(wordList.toSet.intersect(knownSymptoms)))
  
        case (false, false, true) =>
          logChat(username, input)
          parseAnalyticsInput(wordList)
  
        case _ =>
          val msg =
            "Sorry, I didn't understand your request. Try phrases like:\n" +
            "'Take a sleep quiz'\n'I have headache and fever'\n'Show health analytics'"
          appendResponse(msg)
          logChat("Bot", msg)
      }
    }
  }
    
  // Get current user data as an Array[String]
  def loadUserData(username: String): Option[Array[String]] = {
  val file = new File(s"${username}Data.csv")
  if (!file.exists()) return None
  val lines = Source.fromFile(file).getLines().toList
  if (lines.length < 2) return None
  val data = lines(1).split(",", -1)  
  Some(data.padTo(17, ""))           
}

// Save updated data back to file
def saveUserData(username: String, data: Array[String]): Unit = {
  val file = new File(s"${username}Data.csv")
  val header =
    "LoginTimes," +
    "SleepQuizCount,SleepLastResult,SleepAvgScore," +
    "FitnessQuizCount,FitnessLastResult,FitnessAvgScore," +
    "MentalQuizCount,MentalLastResult,MentalAvgScore," +
    "HealthQuizCount,HealthLastResult,HealthAvgScore," +
    "NutritionQuizCount,NutritionLastResult,NutritionAvgScore," +
    "LastRecordedSymptoms"

  val writer = new PrintWriter(new BufferedWriter(new FileWriter(file)))
  writer.println(header)
  writer.println(data.padTo(17, "").mkString(","))
  writer.close()
}

// Create a data file for new user
def createUserDataFile(username: String): Unit = {
  val file = new File(s"${username}Data.csv")
  if (!file.exists()) {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(file)))

    val header =
      "LoginTimes," +
      "SleepQuizCount,SleepLastResult,SleepAvgScore," +
      "FitnessQuizCount,FitnessLastResult,FitnessAvgScore," +
      "MentalQuizCount,MentalLastResult,MentalAvgScore," +
      "HealthQuizCount,HealthLastResult,HealthAvgScore," +
      "NutritionQuizCount,NutritionLastResult,NutritionAvgScore," +
      "LastRecordedSymptoms"

    val initialData = "0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, "
    writer.println(header)
    writer.println(initialData)
    writer.close()
  }
}

// Update login counter
def updateLogin(username: String): Unit = {
  loadUserData(username) match {
    case Some(data) =>
      val loginTimes = data(0).toInt + 1
      data(0) = loginTimes.toString
      saveUserData(username, data)
    case None =>
      println(s"User file not found. Creating new file...")
      createUserDataFile(username)
      updateLogin(username)
  }
}

// Update quiz counter, last result, and average score
def updateQuiz(username: String, quizType: String, result: Int): Unit = {
  val indices = Map(
    "sleep_quality" -> (1, 2, 3),
    "fitness" -> (4, 5, 6),
    "mental_health" -> (7, 8, 9),
    "health_knowledge" -> (10, 11, 12),
    "nutrition" -> (13, 14, 15)
  )

  loadUserData(username) match {
    case Some(data) =>
      indices.get(quizType.toLowerCase) match {
        case Some((countIdx, resultIdx, avgIdx)) =>
          val count = data(countIdx).toInt + 1
          val lastAvg = data(avgIdx).toDoubleOption.getOrElse(0.0)
          val newAvg = ((lastAvg * (count - 1)) + result) / count

          data(countIdx) = count.toString
          data(resultIdx) = result.toString
          data(avgIdx) = f"$newAvg%.2f"
          saveUserData(username, data)

        case None =>
          println("Invalid quiz type.")
      }
    case None =>
      println(s"User file not found. Creating new file...")
      createUserDataFile(username)
      updateQuiz(username, quizType, result)
  }
}

// Update user latest symptoms
def updateSymptoms(username: String, symptoms: List[String]): Unit = {
  loadUserData(username) match {
    case Some(data) =>
      val updatedData = data.padTo(17, "")
      updatedData(16) = symptoms.mkString("-")
      saveUserData(username, updatedData)

    case None =>
      println(s"User file not found. Creating new file...")
      createUserDataFile(username)
      updateSymptoms(username, symptoms)
  }
}
def getAnswer(): String = {
  val input = getNextInput().getOrElse("").toLowerCase.trim
  if (input == "exit" || input == "done") {
    logChat(username, input)
    appendResponse("Quiz ended early. Your responses have been saved.")
    logChat("Bot", "Quiz ended early. Your responses have been saved.")
    throw new Exception("QuizTerminated")
  }
  input
}

  
  // generates different quiz types
  def GenerateQuiz(quizType: String): Unit = {
  def extractQuestions(lines: List[String], quizType: String): List[String] = {
    val startMarker = s"[$quizType]"
    val endMarkers = Set("[sleep_quality]", "[fitness]", "[mental_health]", "[nutrition]", "[health_knowledge]") - startMarker
    lines.dropWhile(_ != startMarker)
      .tail
      .takeWhile(line => !endMarkers.exists(line.startsWith))
      .filter(_.nonEmpty)
  }

  val lines = Source.fromFile("quizzes.txt").getLines().toList
  val questions = extractQuestions(lines, quizType)
  val selectedQuestions = Random.shuffle(questions).take(5)

  val csvWriter = new PrintWriter(new FileWriter(s"${username}.csv", true))
  csvWriter.write(s"\nQuiz Type: $quizType\n")

  var score = 0
  var totalPossible = 0
  val wrongAnswers = ListBuffer[String]()
  try{
  breakable {
    for (n <- selectedQuestions) {
      appendResponse(n)
      logChat("Bot", n)
      var answer = getAnswer()
      logChat(username, answer)
      csvWriter.write(s"Q: $n\n")

      if (n.contains("scale of 1-5")) {
        while (!(1 to 5).contains(answer.toIntOption.getOrElse(0)) && !idkSynonyms.contains(answer)) {
          val msg = "Please enter a number between 1 and 5"
          appendResponse(msg)
          logChat("Bot", msg)
          answer = getAnswer()
          logChat(username, answer)
        }
        score += answer.toInt
        totalPossible += 5

      } else if (n.contains("(Enter number)")) {
        while (!idkSynonyms.contains(answer) && answer.toIntOption.isEmpty) {
          val msg = "Please enter a valid number or just type idk."
          appendResponse(msg)
          logChat("Bot", msg)
          answer = getAnswer()
          logChat(username, answer)
        }
        score += answer.toInt
        totalPossible += 10

      } else if (n.contains("Select from the following")) {
        val answerLines = Source.fromFile("Health_Knowledge answers.csv").getLines().toList
        val index = answerLines.indexWhere(_.trim.equalsIgnoreCase(n.trim))
        val synonyms = if (index != -1 && index + 1 < answerLines.length) {
          answerLines(index + 1).toLowerCase.trim.split(",").map(_.trim).toList
        } else Nil
        val Wrongsynonyms = if (index != -1 && index + 2 < answerLines.length) {
          answerLines(index + 2).toLowerCase.trim.split(",").map(_.trim).toList
        } else Nil
        while (!Wrongsynonyms.contains(answer) && !synonyms.contains(answer)) {
          val msg = s"Please enter one of the valid options (e.g., a, b, c, d or synonymous term)"
          appendResponse(msg)
          logChat("Bot", msg)
          answer = getAnswer()
          logChat(username, answer)
        }

        if (synonyms.contains(answer)) {
          score += 1
        }
        else {
            wrongAnswers += s"❌ Question: $n\n✅ Correct Answer: ${synonyms.headOption.getOrElse("Unknown")}"
        }

        totalPossible += 1

      } else {
        while (!yesSynonyms.contains(answer) && !noSynonyms.contains(answer) && !idkSynonyms.contains(answer)) {
          val msg = "Please write (Yes/No)"
          appendResponse(msg)
          logChat("Bot", msg)
          answer = getAnswer()
          logChat(username, answer)
        }

        if (quizType == "health_knowledge") {
          val answerLines = Source.fromFile("Health_Knowledge answers.csv").getLines().toList
          val index = answerLines.indexWhere(_.trim.equalsIgnoreCase(n.trim))

          if (index != -1 && index + 1 < answerLines.length) {
            val correctAnswer = answerLines(index + 1).toLowerCase.trim
            if ((correctAnswer == "yes" && yesSynonyms.contains(answer)) ||
                (correctAnswer == "no" && noSynonyms.contains(answer))) {
              score += 1
            } else {
              wrongAnswers += s"❌ Question: $n\n✅ Correct Answer: $correctAnswer"
            }
          }
          totalPossible += 1
        } else {
          if (yesSynonyms.contains(answer)) score += 1
          totalPossible += 1
        }
      }

      csvWriter.write(s"A: $answer\n")
    }
  }

  for (wa <- wrongAnswers) {
    appendResponse(wa)
    logChat("Bot",wa)
  }

  val percentage = (score.toDouble / totalPossible) * 100
  csvWriter.write(s"Score: $score/$totalPossible (${percentage.toInt}%)\n\n")
  csvWriter.close()

  val msg = quizType match {
    case "sleep_quality" =>
      if (percentage <= 60) "You may not be getting enough quality sleep. Consider improving your bedtime routine."
      else "Your sleep habits seem healthy! Keep it up."

    case "fitness" =>
      if (percentage >= 60) "You're staying active—great job!"
      else "You may need more physical activity. Try small changes like walking more."

    case "mental_health" =>
      if (percentage <= 60) "You might be experiencing stress or low mood. Consider self-care or speaking to a professional."
      else "Your mental well-being seems balanced. Keep prioritizing yourself!"

    case "health_knowledge" =>
      val scoreMsg = s"You scored $score out of $totalPossible (${percentage.toInt}%)."
      val feedbackMsg = if (percentage >= 60)
        "You're well-informed about health—great!"
      else
        "You may benefit from learning more about basic health practices."
      s"$scoreMsg\n$feedbackMsg"

    case "nutrition" =>
      if (percentage >= 60) "Your diet seems balanced and healthy!"
      else "Your nutrition could use improvement. Try adding more whole foods."

    case _ => "Unknown quiz type."
  }
  println(percentage)
  var saved_percentage= percentage
  if(quizType=="sleep_quality"||quizType=="mental_health" ){
      saved_percentage=100-percentage;
    }
  updateQuiz(username, quizType, saved_percentage.toInt)
  appendResponse(msg)
  logChat("Bot", msg)
  }catch{
      case e: Exception if e.getMessage == "QuizTerminated" =>

  }
}
  

  // displays quiz options
  def DisplayQuizzesOptions(): Unit = {
    msg = "Cool, you can choose a quiz from the following (write 1, 2, 3, 4 or 5):\n1- Sleep Quality Quiz\n2- Fitness & Activity Level Quiz\n3- Mental Health Self-Assessment\n4- General Health Knowledge Quiz\n5- Nutrition & Diet Quiz"
    appendResponse(msg)
    msg.split("\n").filter(_.trim.nonEmpty).foreach { line =>
    logChat("Bot", line.trim)
    }

    val q1 = List("sleep", "1", "sleep quality quiz", "quiz 1", "one", "rest", "resting", "sleep quiz", "quality of sleep", "sleep check", "how well i sleep")
    val q2 = List("fitness", "2", "fitness & activity level quiz", "quiz 2", "two", "activity", "workout", "exercise", "active", "fitness quiz", "how fit i am", "fitness level", "exercise habits", "step count", "physical activity")
    val q3 = List("mental", "3", "mental health self-assessment", "quiz 3", "three", "mental health", "stress", "depression", "anxiety", "mental check", "mind", "how i feel", "mood", "mental state", "emotions", "emotional health", "mental quiz")
    val q4 = List("general", "4", "general health knowledge quiz", "quiz 4", "four", "health knowledge", "health facts", "body knowledge", "wellness quiz", "basic health", "common health", "health info", "general quiz", "basic quiz", "health basics")
    val q5 = List("nutrition", "5", "nutrition & diet quiz", "quiz 5", "five", "diet", "food", "eating habits", "healthy eating", "nutrition quiz", "nutrition check", "meal habits", "what i eat", "calories", "diet quiz", "eating")

    val input = getAnswer()
    logChat(username, input)

    input match {
      case i if q1.contains(i) => GenerateQuiz("sleep_quality")
      case i if q2.contains(i) => GenerateQuiz("fitness")
      case i if q3.contains(i) => GenerateQuiz("mental_health")
      case i if q4.contains(i) => GenerateQuiz("health_knowledge")
      case i if q5.contains(i) => GenerateQuiz("nutrition")
      case _ =>
        msg = "Invalid choice. Please enter a valid quiz option (1 to 5)."
        appendResponse(msg)
        logChat("Bot", msg)
        DisplayQuizzesOptions()
    }
}

   // detects if user input is a quiz
  def parseQuizInput(list: List[String]): Unit = {
    val sleepSynonyms = Set("sleep", "insomnia", "rest", "dream", "tired", "drowsy")
    val fitnessSynonyms = Set("fitness", "exercise", "workout", "gym", "active", "sport")
    val mentalSynonyms = Set("mental", "stress", "anxiety", "depression", "mood", "mind")
    val generalSynonyms = Set("general", "basic", "common", "overall", "health")
    val nutritionSynonyms = Set("nutrition", "diet", "food", "eating", "calories", "vitamin")
    list match {
        case _ if list.exists(sleepSynonyms.contains) => GenerateQuiz("sleep_quality")
        case _ if list.exists(fitnessSynonyms.contains) => GenerateQuiz("fitness")
        case _ if list.exists(mentalSynonyms.contains)  => GenerateQuiz("mental_health")
        case _ if list.exists(generalSynonyms.contains) => GenerateQuiz("health_knowledge")
        case _ if list.exists(nutritionSynonyms.contains) => GenerateQuiz("nutrition")
        case _ => DisplayQuizzesOptions()
    }
  }


 // analyze users  past data
def parseAnalyticsInput(list: List[String]): Unit = {
  if (username.isEmpty) {
    appendResponse("⚠ No user is currently logged in.")
    return
  }

  val wordList = list.map(_.toLowerCase)

  val sleepKeywords     = Set("sleep", "rest", "nap", "bed", "tired")
  val fitnessKeywords   = Set("fitness", "exercise", "workout", "gym", "physical")
  val mentalKeywords    = Set("mental", "mind", "stress", "anxiety", "emotion")
  val healthKeywords    = Set("health", "general", "body", "checkup", "overall")
  val nutritionKeywords = Set("nutrition", "food", "diet", "eating", "calories")
  val symptomsKeywords  = Set("symptom", "symptoms", "sick", "illness", "unwell")

  val showSleep     = wordList.exists(sleepKeywords.contains)
  val showFitness   = wordList.exists(fitnessKeywords.contains)
  val showMental    = wordList.exists(mentalKeywords.contains)
  val showHealth    = wordList.exists(healthKeywords.contains)
  val showNutrition = wordList.exists(nutritionKeywords.contains)
  val showSymptoms  = wordList.exists(symptomsKeywords.contains)

  val showAll = !showSleep && !showFitness && !showMental && !showHealth && !showNutrition && !showSymptoms

  def recommend(category: String, avgScore: Double): Unit = {
  if (avgScore <= 60) {
    val tips = category match {
      case "Sleep" =>
        "Your sleep habits could use some improvement. Try sticking to a regular bedtime and avoiding screens before bed to sleep better."
      case "Fitness" =>
        "It looks like your fitness level could be better. Try to include more physical activity in your routine—it really helps."
      case "Mental Health" =>
        "Your mental health score suggests there’s room to improve. Practicing mindfulness or talking to someone can make a difference."
      case "General Health" =>
        "There’s some room to grow in your general health knowledge. Learning more about how your body works can help you make smarter choices."
      case "Nutrition" =>
        "Your nutrition could be improved. Aim for more balanced meals and try to include a variety of nutrients in your diet."
      case _ =>
        ""
    }
    appendResponse(s"💡 Based on your average score, there's room to improve your $category. $tips")
  } else {
    appendResponse(s"🎉 You're doing well in the $category area! Keep up the good habits!")
  }
}



  loadUserData(username) match {
    case Some(data) =>
      val loginTimes        = data(0)
      val sleepCount        = data(1).toInt
      val sleepLast         = data(2)
      val sleepAvg          = data(3)
      val fitnessCount      = data(4).toInt
      val fitnessLast       = data(5)
      val fitnessAvg        = data(6)
      val mentalCount       = data(7).toInt
      val mentalLast        = data(8)
      val mentalAvg         = data(9)
      val healthCount       = data(10).toInt
      val healthLast        = data(11)
      val healthAvg         = data(12)
      val nutritionCount    = data(13).toInt
      val nutritionLast     = data(14)
      val nutritionAvg      = data(15)
      val symptoms          = data(16)
      
      
      appendResponse(s"👋 Hey $username! Here's a summary of your recent health quiz activity:")

      if (showAll) appendResponse(s"🔐 You've logged in $loginTimes times so far.")
      
        if (showAll) {
          val quizCounts = List(
            ("Sleep", sleepCount),
            ("Fitness", fitnessCount),
            ("Mental", mentalCount),
            ("Health", healthCount),
            ("Nutrition", nutritionCount)
          )

          val topQuizOpt = quizCounts.filter(_._2 > 0).sortBy(_._2).headOption

          topQuizOpt.foreach { case (category, count) =>
            val icon = category match {
              case "Sleep"     => "🛌"
              case "Fitness"   => "💪"
              case "Mental"    => "🧠"
              case "Health"    => "🏥"
              case "Nutrition" => "🥗"
              case _           => ""
            }
            appendResponse(s"\n🏆 You’ve been most active in the $icon $category category with $count quizzes!")
          }
        }

      def handleQuiz(category: String, count: Int, last: String, avg: String, show: Boolean): Unit = {
        if (show || showAll) {
          if (count > 0 || !showAll) {
            appendResponse(s"\n${category match {
              case "Sleep"     => "🛌 Sleep"
              case "Fitness"   => "💪 Fitness"
              case "Mental"    => "🧠 Mental Health"
              case "Health"    => "🏥 General Health"
              case "Nutrition" => "🥗 Nutrition"
            }}")

            if (count > 0) {
              appendResponse(s"You’ve taken the $category quiz $count times.")
              appendResponse(s"Your most recent score was $last%.")
              appendResponse(s"Your average score across all attempts is $avg%.")
              recommend(category, avg.toDoubleOption.getOrElse(0.0))
            } else {
              appendResponse(s"You haven’t taken the $category quiz yet. Give it a try to start tracking!")
            }
          }
        }
      }

      handleQuiz("Sleep", sleepCount, sleepLast, sleepAvg, showSleep)
      handleQuiz("Fitness", fitnessCount, fitnessLast, fitnessAvg, showFitness)
      handleQuiz("Mental", mentalCount, mentalLast, mentalAvg, showMental)
      handleQuiz("Health", healthCount, healthLast, healthAvg, showHealth)
      handleQuiz("Nutrition", nutritionCount, nutritionLast, nutritionAvg, showNutrition)

      if ((showSymptoms || showAll) && symptoms.trim.nonEmpty) {
        appendResponse(s"\n📝 Last Recorded Symptoms: $symptoms")
      }

      appendResponse("\nWould you like to take another quiz now? (yes/no)")
      val response = getNextInput().getOrElse("").toLowerCase.trim
      if ( yesSynonyms.contains(response)) {
        DisplayQuizzesOptions()
        // Handle quiz selection
      } else {
        appendResponse("No worries! Take care and stay healthy! 😊")
      }

    case None =>
      appendResponse("⚠ We couldn't find any data for your account.")
  }
}
  

  def initializeChatFile(): Unit = {
  val writer = new PrintWriter(new FileWriter(s"$username chathistory.csv", false)) // overwrite once
  writer.close()
}

  def logChat(sender: String, message: String): Unit = {
  val writer = new PrintWriter(new FileWriter(s"$username chathistory.csv", true)) // always append
  writer.write(s"$sender: $message\n")
  writer.close()
}



 // greets user warmly
 def GreetUser(username: String): String = {
  val maybeData = loadUserData(username)

  val initialMsg = maybeData match {
    case Some(data) if data.length > 16 && data(16).trim.nonEmpty =>
      s"Hey $username, welcome back to Shadia Bot!\nI see you previously reported these symptoms: ${data(16)}.\nHow are you feeling now?"
    case _ =>
      s"Hey $username, how are you feeling today?"
  }

  appendResponse(initialMsg)
  initialMsg.split("\n").filter(_.trim.nonEmpty).foreach { line =>
    logChat("Bot", line.trim)
  }

  print("\n> ")
  val feeling = getNextInput().getOrElse("").toLowerCase.trim
  logChat(username, feeling)

  // Define synonyms for good and bad feelings
  val goodSynonyms = Set("good", "well", "fine", "great", "happy", "okay", "awesome", "excellent", "better", "fantastic","amazing","tarsh","zay el fol","alhamdulillah")
  val badSynonyms  = Set("bad", "not good", "sad", "unwell", "tired", "not well", "sick", "depressed", "worried", "anxious","not fine","not happy","not okay","exhausted","mesh kwayesa","mesh 3arfa")

  // Determine response based on feeling
  val feelingResponse = 
    if (badSynonyms.exists(feeling.contains)) {
      s"I'm sorry to hear that, $username. I'm here to help you feel better."
    } else if (goodSynonyms.exists(feeling.contains)) {
      s"That's good to hear, $username! I'm Shadia, your health assistant."
    } else {
      s"Thanks for sharing, $username. I'm here to support you."
    }

  val intro =
    """I'm here to help you with a variety of things:
      |- Diagnose your symptoms 🩺
      |- Answer health-related questions 🧠
      |- Provide information about diseases 🦠
      |- Offer interactive quizzes:
      |  1. Sleep Quality 😴
      |  2. Fitness & Activity Level 💪
      |  3. Mental Health Self-Assessment 🧘
      |  4. General Health Knowledge 🧬
      |  5. Nutrition & Diet 🥗
      |- Generate a personalized health analytics report based on your responses and quiz results 📊
      |""".stripMargin

  val followUpMsg = s"$feelingResponse\n\n$intro\n"

  appendResponse(followUpMsg)
  followUpMsg.split("\n").filter(_.trim.nonEmpty).foreach { line =>
  logChat("Bot", line.trim)
}

  feeling  
}




// gets last chat session from user chat history file 
def getLastChatSession(username: String): JList[String] = {
  val file = new File(s"${username} chathistory.csv")
  if (!file.exists()) return null

  val rawLines = Source.fromFile(file).getLines().toList
  val result = new java.util.ArrayList[String]()
  rawLines.foreach(line => result.add(line.trim))
  result
}

// outputs the last chat session on screen 
def printLastChatSession(username: String): Unit = {
  val lines = getLastChatSession(username)

  if (lines == null || lines.isEmpty) {
    appendResponse("No previous chat session found.")
    return
  }

  appendResponse("\n--- Last Chat Session ---")

  val grouped = scala.collection.mutable.ListBuffer.empty[(String, String)]
  var currentSpeaker: Option[String] = None
  var currentMessage = new StringBuilder()

  def flush(): Unit = {
    currentSpeaker.foreach { speaker =>
      val msg = currentMessage.toString().trim
      if (msg.nonEmpty) grouped += ((speaker, msg))
    }
    currentSpeaker = None
    currentMessage = new StringBuilder()
  }

  val lowercaseUsername = username.toLowerCase

  lines.forEach { line =>
    val trimmed = line.trim
    val parts = trimmed.split(":", 2)

    if (parts.length == 2 && (parts(0).equalsIgnoreCase("bot") || parts(0).equalsIgnoreCase(username))) {
      flush()
      currentSpeaker = Some(parts(0).trim.toLowerCase)
      currentMessage.append(parts(1).trim)
    } else {
      if (currentMessage.nonEmpty) currentMessage.append("\n")
      currentMessage.append(trimmed)
    }
  }

  flush()

  if (grouped.nonEmpty) {
    grouped.foreach {
      case ("bot", msg)               => appendResponse(s"Shadia: $msg")
      case (lowercaseUsername, msg) => appendResponse(s"User: $msg")
    
    }
  } else {
    appendResponse("No valid messages to display.")
  }

  appendResponse("--- End of Session ---\n")
}



}

object HealthBotGateway {
  def main(args: Array[String]): Unit = {
    HealthBot.loadSynonymsCSV("synonyms.csv")
    HealthBot.loadSymptomsCSV("symptoms_to_disease.csv")
    HealthBot.loadDiseaseInfoCSV("disease_info.csv")
    
    

    val gateway = new GatewayServer(HealthBot)
    gateway.start()
    println("Py4J Gateway Server Started")

    new Thread(() => {
      while (true) {
        val input = HealthBot.getNextInput().getOrElse("") 
        if (input.nonEmpty) {
          HealthBot.HandleUserInput(input)
        }
      }
    }).start()

  
    while (true) {
      Thread.sleep(10000)
    }
  }
}