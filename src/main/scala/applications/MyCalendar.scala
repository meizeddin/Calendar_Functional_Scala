package applications

import lib.picture.Picture

import lib.picture.Picture.*

object MyCalendar {

  /*
  A calendar stores, and displays, events that are scheduled on specific
  days. Each event has a date, a time, and a description.
  */

  case class Date(year: Int, month: Int, day: Int)
  case class Time(hour: Int, minute: Int) {
    def toPicture: Picture = Picture(f"$hour%02d:$minute%02d")
  }
  case class Event(date: Date, time: Time, desc: String)

  /**
   * The columns of the calendar are set to a given width.
   */
  val EventWidth: Int = 14

  /**
   * The place where the events are stored.
   */
  val EventsPath: String = "dat/events.txt"

  /**
   * Returns the number of days in a month. Deals with leap years.
   *
   * @return The number of days in a given month for a given year.
   */
  def daysInMonth(year: Int, month: Int): Int = month match {
    case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
    case 4 | 6 | 9 | 11 => 30
    case _ => if (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0) then 29 else 28
  }

  /**
   * The names of the months are stored in a Map relating month numbers (1..) to names.
   * To get the picture representing "JANUARY", write nameOfMonth(1)
   */
  val nameOfMonth: Map[Int, Picture] = Map(
    1 -> Picture("JANUARY"),
    2 -> Picture("FEBRUARY"),
    3 -> Picture("MARCH"),
    4 -> Picture("APRIL"),
    5 -> Picture("MAY"),
    6 -> Picture("JUNE"),
    7 -> Picture("JULY"),
    8 -> Picture("AUGUST"),
    9 -> Picture("SEPTEMBER"),
    10 -> Picture("OCTOBER"),
    11 -> Picture("NOVEMBER"),
    12 -> Picture("DECEMBER")
  )

  /**
   * Days of the week are numbered
   * 0 = Monday
   * 1 = Tuesday
   * 2 = Wednesday
   * 3 = Thursday
   * 4 = Friday
   * 5 = Saturday
   * 6 = Sunday
   *
   * Uses an algorithm published by Tomohiko Sakamoto in 1993
   * See http://www.faqs.org/faqs/sci-math-faq/dayWeek/
   * The algorithm assumes the first day (0) is Sunday. This
   * method adjusts the result so that the first day (0) is
   * Monday.
   *
   * @return the day of the week for a given year/month/day
   *         where 0=Monday, 1=Tuesday,..., 6=Sunday
   */
  def getDayOfWeek(year: Int, month: Int, day: Int): Int = {
    val t = List(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
    val d = day
    val m = month
    val y = if month < 3 then year - 1 else year
    val answerSundayEq0 = (y + y / 4 - y / 100 + y / 400 + t(m - 1) + d) % 7
    val answerMondayEq0 = (answerSundayEq0 + 6) % 7
    answerMondayEq0
  }


  /**
   * The names of the days are stored as a sequence of pictures. To ensure that all the
   * headers in the calendar are the same width, the day names are fixed to the given
   * constant EVENT_WIDTH.
   */
  val namesOfDays: Seq[Picture] =
    Seq("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      .map(Picture(_).fixWidth(EventWidth))

  /**
   * Simple method to read the events from a text file. Absolutely no validation is
   * included. The text file is assumed to be formatted correctly. It reads events
   * by line in the format:
   * year month day hour minute description
   * E.g.
   * 2023 2  22  09  00  CTEC3904 lecture in MS1.02
   *
   * Blank lines are ignored. Multiple spaces are ignored.
   *
   * @param filename The pathname of the file of event data. In IntelliJ this is relative
   *                 to the project root. Thus, "/dat/events.txt" would refer to
   * {{{
   *                 v ProjectName
   *                     > dat
   *                         events.txt
   * }}}
   * @return A sequence of events.
   */
  def readEventsFromFile(filename: String): Seq[Event] =
    import java.io.File
    import scala.io.{BufferedSource, Source}
    val fileHandle: BufferedSource = Source.fromFile(filename)
    val lines: Seq[String] = fileHandle.getLines.toList
    fileHandle.close()
    for
      line <- lines
      if line.nonEmpty
    yield
      val (datetime, description) = line.split("\\s+").toList.splitAt(5)
      val (List(year, month, day), List(hour, minute)) = datetime map (_.toInt) splitAt 3
      Event(Date(year, month, day), Time(hour, minute), description.mkString(" "))

  /**
   * A method to make up random diary events.  Useful for testing.
   * N.B. It over-writes the file at EventsPath
   */
  def makeUpEvents(numberOfEvents: Int, filename: String): Unit = {
    import java.io.File
    import java.io.BufferedWriter
    import java.io.FileWriter
    import scala.io.{BufferedSource, Source}
    val file = new File(filename)
    val fileHandle = new BufferedWriter(new FileWriter(file))
    val r = scala.util.Random
    for (_ <- 1 to numberOfEvents) {
      val yy = r.nextInt(2) + 2023 // Uses dates in 2023 and 2024
      val mm = r.nextInt(12) + 1
      val dd =
        if Set(1, 3, 5, 7, 8, 10, 12) contains mm then
          r.nextInt(31) + 1
        else if Set(4, 6, 9, 11) contains mm then
          r.nextInt(30) + 1
        else if (yy % 4 == 0 && yy % 100 != 0) || (yy % 400 == 0) then
          r.nextInt(29) + 1
        else
          r.nextInt(28) + 1
      val hrs = r.nextInt(14)
      val mins = if r.nextInt(2) == 0 then 0 else 30
      val desc = f"Event-auto-gen ($yy/$mm/$dd @ $hrs%02d:$mins%02d)"
      fileHandle.write(f"$yy%4d$mm%4d$dd%4d  $hrs%02d $mins%02d $desc\n")
    }
    fileHandle.close()
  }

  /** THE COURSEWORK METHOD
   * **********************************************************************************
   * Produces a picture (ready for display on the output console) of the given month in
   * the calendar.  The events are filtered so that only those relevant for the given
   * month are displayed.
   ************************************************************************************
   * @param year   The calendar year. (e.g. 2023)
   * @param month  The calendar month (1 = JANUARY, etc.)
   * @param events The sequence of (all) events. It will need to be filtered to obtain
   *               those events relevant for this month and this year.
   * @return A nicely formatted calendar page that contains a view of the given
   *         month in the given year.
   */
  def displayMonth(year: Int, month: Int, events: Seq[Event]): Picture = {

    /*
     * Get the name of the month as a picture.
     */
    val monthName: Picture = nameOfMonth(month)

    /*
     * Determines the number of days in the given month at a given year.
     */
    val daysInThisMonth: Int = daysInMonth(year, month)

    /*
     * Determines the day of the week (0-6)(monday/sunday) for the first day of the given month.
     */
    val firstDayOfMonth: Int = getDayOfWeek(year, month, 1)

    /*
     * Creates a list of the days in the previous month as Integers.
     * Using a map converts the list of integers to a list of strings (Important for making Pictures later on).
     * Using takeRight, take the number of days from the previous month in the week leading up to the first day of the given month.
     * I.e. if firsDayOfMonth is on Thursday (3), given that the previous month has 31 days, takeRight will take days (29, 30, 31).
     */
    val daysInPreviousMonth: List[String] = (1 to daysInMonth(year, month - 1)).toList
      .map(_.toString)
      .takeRight(firstDayOfMonth)

    /*
     * Creates a list of the days in the current month as Integers.
     * Using a map converts the list of integers to a list of strings (Important for making Pictures later on)
     * Appends to the left a list of negative integers accounting for the first day of the month being mid-week and allowing the distinction between
     * days of the current month and days of the previous month when creating the Pictures later on.
     */
    val daysInThisMonthList: List[String] = (-(daysInPreviousMonth.length-1) to 0).toList.map(_.toString) ++ (1 to daysInThisMonth).toList.map(_.toString)

    /*
     * Creates a list of the weeks in the given month, with each week being a list of the days in that week (7).
     * If you are wondering about the first week being less than 7 days, if it starts mid-week, look at the previous step where
     * we appended the missing days from the previous month. We can append zeros instead, and ignore them using the if statement
     * in tableData. However, it looks nicer this way.
     */
    val weeks: List[List[String]] = daysInThisMonthList.grouped(7).toList

    /*
     * Sorts the sequence of events by minutes then hours.
     * This is helpful for displaying events with the same date in order.
     */
    val sortedByTime: Seq[Event] = events.sortBy(_.time.minute).sortBy(_.time.hour)

    /*
     * Iterates through the sorted sequence of events and filters it by required date (year and month).
     * Then, groups events by date creating a Map of dates and their sequence of events.
     * I.e if two events have the same date it would look like [Date(2024,2,10) -> List(Event1, Event2)]
     */
    val groupedEventsByDate: Map[Date, Seq[Event]] =
      sortedByTime
        .filter(event => (event.date.year == year) && (event.date.month == month))
        .groupBy(_.date)

    /*
     * Takes the Map of [Date, Seq[Event]] and creates a Map of [Date, Seq[Picture]] by using a map function on the event
     * to create two Pictures, one for time and one for description, then combines them together using above to create a
     * single picture.
     */
    val groupedEventsPictures: Map[Date, Seq[Picture]] = groupedEventsByDate.map((date, event) => (date, event map { event =>
      val timePic: Picture = event.time.toPicture
      val descPic: Picture = flow(20, event.desc)
      timePic.above(descPic)
    }))

    /*
     * Takes the Map of [Date, Seq[Picture]] and reduces the sequence of pictures within each Date to a single picture. If
     * there are two or more pictures in a Date then these need to be combined
     * with the picture ("Also") clarifying that it is a second event on the same Date.
     */
    val groupedAndJoinedEventsPictures: Map[Date, Picture] =
      groupedEventsPictures.map((date, events) => (date, events.reduceLeft((p1, p2) => p1.^(Picture("Also"), MID).above(p2))))

    /*
     * Creates a 2D Seq of pictures, needed to create the calender, by iterating through each week in weeks using map,
     * then iterating through each day in a week producing its picture.
     * Each cell/picture containing the day and events for that day.
     * If there is no event at a given day then a blank picture is constructed (Picture(' '))
     * because there must be a picture in every cell.
     * If the day is a positive integer, it's a day in the current month, else it's a day in the previous month.
     */
    val tableData: Seq[Seq[Picture]] = weeks.map { week =>
      week.map(day =>
        val date: Date = Date(year, month, day.toInt)
        val picture: Picture = groupedAndJoinedEventsPictures.getOrElse(date, Picture(""))
          if((day.toInt > 0)) {
            Picture(day).above(Picture("  ")).above(picture)
          }else {
            Picture(daysInPreviousMonth(week.indexOf(day)))
          }
        )
    }

    /*
     * A 2D sequence of pictures representing the entire timetable, with the days of the week at the top and the calendar table below
     */
    val timetable: Seq[Seq[Picture]] =  namesOfDays +: tableData

    // The month name and year at the top of the timetable, with the timetable formatted as a table
    monthName.beside(Picture(s" ${year}")).above(timetable.formatAsTable())
  }

  /**
   * A method to create a set of random events ane write them to the
   * default text file, EventsPath.  To change the number of events
   * simply adjust the number in the parameter list.
   */
  @main def constructRandomEventFile(): Unit =
    makeUpEvents(1000, EventsPath)

  @main def coursework(): Unit =
    /* Read the events from an external file... */
    val events = readEventsFromFile(EventsPath)
    /* Or create the data structure by hand... */
    //    val es = Seq(
    //      Event(Date(2023, 2, 22), Time(9, 0), "CTEC3904 lecture in MS1.02")
    //      //etc.
    //    )
    /* Choose a year/month to display from a sequence of events... */
    println(displayMonth(2024, 2, events))
    //println(getDayOfWeek(2024, 2, 1))
}
