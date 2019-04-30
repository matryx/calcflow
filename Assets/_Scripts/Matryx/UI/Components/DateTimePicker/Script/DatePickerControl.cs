/*****
//	▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
//	█  █▄   █ █▄   █ █▀▀▀▀█          █▀▀▀▀▀ █▀▀▀▀█ █▄  ▄█ █▀▀▀▀▀ █▀▀▀▀█ █▀▀▀▀▀   █
//	█  █ ▀▄ █ █ ▀▄ █ █▄▄▄▄█  ▄▄▄▄▄▄  █  ▄▄▄ █▄▄▄▄█ █ ▀▀ █ █▄▄▄▄  █▄▄▄▄█ █▄▄▄▄▄   █
//	█  █   ▀█ █   ▀█ █    █          █▄▄▄▄█ █    █ █    █ █▄▄▄▄▄ █   ▀▄ ▄▄▄▄▄█   █
//	▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
//		© NNA-Gamers 2017, Man Sanz
*****/
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using UnityEngine.UI;

#if UNITY_EDITOR
	using UnityEditor;
#endif

[Serializable]
public class date{
	public bool fontSizeCustom;
	public InputField day;
	public int fontSizeDay=1;
	public InputField month;
	public int fontSizeMonth=1;
	public InputField year;
	public int fontSizeYear=1;
}

[Serializable]
public class time{
	public bool fontSizeCustom;
	public InputField hour;
	public int fontSizeHour=1;
	public InputField minute;
	public int fontSizeMinute=1;
	public InputField second;
	public int fontSizeSecond=1;
}

public class DatePickerControl : MonoBehaviour {
	public DateTime fechaVisual;
	public DateTime	fecha;
	public static string dateStringFormato;
	public Text dateText;

	public enum nFormato{
		Default,
		dd_MM_yyyy,
		MM_dd_yyyy,
		yyyy_MM_dd,
		yyyy_dd_MM,
		ddd_dd_MM_yyyy,
		ddd_MM_dd_yyyy,
		ddd,
		dddd,
		HH_mm,
		HH_mm_ss,
		hh_mmtt,
		hh_mm_sstt,
		custom
	}

	public nFormato formato = nFormato.Default;
	[HideInInspector]
	public string formatoCustom="dddd dd/MM/yyyy HH:mm:ss";
	[HideInInspector]
	public char separator='/';

	[HideInInspector]
	public bool dateOn;
	[HideInInspector]
	public date inputFieldDate;
	[HideInInspector]
	public bool timeOn;
	[HideInInspector]
	public time inputFieldTime;

	// Use this for initialization
	void Start () {
		if (dateOn && inputFieldDate.fontSizeCustom) {
			if(inputFieldDate.day!=null)
				inputFieldDate.day.transform.GetChild (1).GetComponent<Text> ().fontSize = inputFieldDate.fontSizeDay;

			if(inputFieldDate.month!=null)
				inputFieldDate.month.transform.GetChild (1).GetComponent<Text> ().fontSize = inputFieldDate.fontSizeMonth;

			if(inputFieldDate.year!=null)
				inputFieldDate.year.transform.GetChild (1).GetComponent<Text> ().fontSize = inputFieldDate.fontSizeYear;
		}
		if (timeOn && inputFieldTime.fontSizeCustom) {
			if(inputFieldTime.hour!=null)
				inputFieldTime.hour.transform.GetChild (1).GetComponent<Text> ().fontSize = inputFieldTime.fontSizeHour;

			if(inputFieldTime.minute!=null)
				inputFieldTime.minute.transform.GetChild (1).GetComponent<Text> ().fontSize = inputFieldTime.fontSizeMinute;

			if(inputFieldTime.second!=null)
				inputFieldTime.second.transform.GetChild (1).GetComponent<Text> ().fontSize = inputFieldTime.fontSizeSecond;
		}
		fecha = DateTime.Now;
        fechaVisual = DateTime.Now;
		actualizarFecha ();
	}
	
	// Update is called once per frame
	void Update () {
		if (dateText != null) {
			if (formato == nFormato.Default) {
				dateText.text = dateStringFormato = fechaVisual.ToString ();
			} else if(formato == nFormato.custom){
				if (formatoCustom == "") {
					dateText.text = dateStringFormato = fechaVisual.ToString ();
				} else {
					dateText.text = dateStringFormato = fechaVisual.ToString (formatoCustom);
				}
			} else{
				dateText.text = dateStringFormato = fechaVisual.ToString (formato.ToString().Replace('_',separator));
			}
		}
	}

	public void InputDateTime(){
		int d = -1, M = -1, y = -1, h = -1, m = -1, s = -1;

		if (inputFieldDate.day != null && dateOn) {
			d = int.Parse (inputFieldDate.day.text);
		} else {
			d = System.DateTime.Now.Day;
		}
		if (inputFieldDate.month != null && dateOn) {
			M = int.Parse (inputFieldDate.month.text);
		} else {
			M = System.DateTime.Now.Month;
		}
		if (inputFieldDate.year != null && dateOn) {
			y = int.Parse (inputFieldDate.year.text);
		} else {
			y = System.DateTime.Now.Year;
		}
		if (inputFieldTime.hour != null && timeOn) {
			h = int.Parse (inputFieldTime.hour.text);
		} else {
			h = System.DateTime.Now.Hour;
		}
		if (inputFieldTime.minute != null && timeOn) {
			m = int.Parse (inputFieldTime.minute.text);
		} else {
			m = System.DateTime.Now.Minute;
		}
		if (inputFieldTime.second != null && timeOn) {
			s = int.Parse (inputFieldTime.second.text);
		} else {
			s = System.DateTime.Now.Second;
		}
		if (dateOn && timeOn) {
			try {
				fecha = new DateTime (y, M, d, h, m, s);
				fechaVisual = fecha;
			} catch {
				if (d > 28 && M == 2) {
					fecha = new DateTime (y, 2, 28);
				} else {
					try {
						diaMin ();
					} catch {
					}
				}
				fecha = new DateTime (fechaVisual.Year, fechaVisual.Month, fechaVisual.Day, 
					fechaVisual.Hour, fechaVisual.Minute, fechaVisual.Second);
				Debug.Log ("Fecha Invalida");
				actualizarFecha ();
			}
		} else if (dateOn) {
			try {
				fecha = new DateTime (y, M,d,
					System.DateTime.Now.Hour, System.DateTime.Now.Minute,System.DateTime.Now.Second);
				fechaVisual = fecha;
			} catch {
				if (d > 28 && M == 2) {
					fecha = new DateTime (y, 2, 28);
				} else {
					try {
						diaMin ();
					} catch {
					}
				}
				Debug.Log ("Fecha Invalida");
				actualizarFecha ();
			}
		} else if (timeOn) {
			try {
				fecha = new DateTime (y, M, d, h, m,s);
				fechaVisual = fecha;
			} catch {
				fecha = new DateTime (fechaVisual.Year, fechaVisual.Month, fechaVisual.Day, 
					fechaVisual.Hour, fechaVisual.Minute, fechaVisual.Second);
				
				Debug.Log ("Fecha Invalida");
				actualizarFecha ();
			}
		}
		actualizarFecha ();
	}
	//		© NNA-Gamers 2017, Man Sanz
	public void actualizarFecha(){
		if (dateOn) {
			if (inputFieldDate.day != null) {
				inputFieldDate.day.text = "" + fecha.Day;
			}
			if (inputFieldDate.month != null) {
				inputFieldDate.month.text = "" + fecha.Month;
			}
			if (inputFieldDate.year != null) {
				inputFieldDate.year.text = "" + fecha.Year;
			}
		}
		if (timeOn) {
			if (inputFieldTime.hour != null) {
				inputFieldTime.hour.text = "" + fecha.Hour;
			}
			if (inputFieldTime.minute != null) {
				inputFieldTime.minute.text = "" + fecha.Minute;
			}
			if (inputFieldTime.second != null) {
				inputFieldTime.second.text = "" + fecha.Second;
			}
		}
		fechaVisual = fecha;
	}

	public void diaMax(){
		try{
			fecha = new DateTime (fecha.Year, fecha.Month, fecha.Day + 1,fecha.Hour,fecha.Minute,fecha.Second);
		}catch{
			Debug.Log ("(+) No hay mas dias");
		}
		actualizarFecha ();
	}

	public void diaMin(){
		try{
			fecha = new DateTime (fecha.Year, fecha.Month, fecha.Day - 1,fecha.Hour,fecha.Minute,fecha.Second);
		}catch{
			Debug.Log ("(-) No hay mas dias");
		}
		actualizarFecha ();
	}

	public void mesMax(){
		try{
			if(fecha.Day > 28 && fecha.Month == 1){
				fecha = new DateTime (fecha.Year, fecha.Month, 28,fecha.Hour,fecha.Minute,fecha.Second);
			}
			fecha = new DateTime (fecha.Year, fecha.Month + 1, fecha.Day,fecha.Hour,fecha.Minute,fecha.Second);
		}catch{
			if(fecha.Day > 1 && fecha.Month < 12){
				try{
					diaMin ();
				}catch{
				}
			}
			Debug.Log ("(+) No hay mas meses");
		}
		actualizarFecha ();
	}

	public void mesMin(){
		try{
			if(fecha.Day > 28 && fecha.Month == 3){
				fecha = new DateTime (fecha.Year, fecha.Month, 28,fecha.Hour,fecha.Minute,fecha.Second);
			}
			fecha = new DateTime (fecha.Year, fecha.Month - 1, fecha.Day,fecha.Hour,fecha.Minute,fecha.Second);
		}catch{
			if(fecha.Day > 1 && fecha.Month > 1){
				try{
					diaMin ();
				}catch{
				}
			}
			Debug.Log ("(-) No hay mas meses");
		}
		actualizarFecha ();
	}

	public void yearMax(){
		try{
			fecha = new DateTime (fecha.Year + 1, fecha.Month, fecha.Day,fecha.Hour,fecha.Minute,fecha.Second);
		}catch{
			if(fecha.Year < DateTime.MaxValue.Year){
				try{
					diaMin ();
					fecha = new DateTime (fecha.Year + 1, fecha.Month, fecha.Day,fecha.Hour,fecha.Minute,fecha.Second);
				}catch{
				}
			}
			Debug.Log ("(+) No hay mas años");
		}
		actualizarFecha ();
	}

	public void yearMin(){
		try{
			fecha = new DateTime (fecha.Year - 1, fecha.Month, fecha.Day,fecha.Hour,fecha.Minute,fecha.Second);
		}catch{
			if(fecha.Year > 1){
				try{
					diaMin ();
					fecha = new DateTime (fecha.Year - 1, fecha.Month, fecha.Day,fecha.Hour,fecha.Minute,fecha.Second);
				}catch{
				}
			}
			Debug.Log ("(-) No hay mas años");
		}
		actualizarFecha ();
	}

	public void hourMax(){
		try{
			fecha = new DateTime (fecha.Year, fecha.Month, fecha.Day, fecha.Hour+1,fecha.Minute,fecha.Second);
		}catch{
			Debug.Log ("(+) No hay mas horas");
		}
		actualizarFecha ();
	}

	public void hourMin(){
		try{
			fecha = new DateTime (fecha.Year, fecha.Month, fecha.Day, fecha.Hour-1,fecha.Minute,fecha.Second);
		}catch{
			Debug.Log ("(-) No hay mas horas");
		}
		actualizarFecha ();
	}

	public void minuteMax(){
		try{
			fecha = new DateTime (fecha.Year, fecha.Month, fecha.Day, fecha.Hour,fecha.Minute+1,fecha.Second);
		}catch{
			Debug.Log ("(+) No hay mas minutos");
		}
		actualizarFecha ();
	}

	public void minuteMin(){
		try{
			fecha = new DateTime (fecha.Year, fecha.Month, fecha.Day, fecha.Hour,fecha.Minute-1,fecha.Second);
		}catch{
			Debug.Log ("(-) No hay mas minutos");
		}
		actualizarFecha ();
	}

	public void secondMax(){
		try{
			fecha = new DateTime (fecha.Year, fecha.Month, fecha.Day, fecha.Hour,fecha.Minute,fecha.Second+1);
		}catch{
			Debug.Log ("(+) No hay mas segundos");
		}
		actualizarFecha ();
	}

	public void secondMin(){
		try{
			fecha = new DateTime (fecha.Year, fecha.Month, fecha.Day, fecha.Hour,fecha.Minute,fecha.Second-1);
		}catch{
			Debug.Log ("(-) No hay mas segundos");
		}
		actualizarFecha ();
	}

	public void fechaHoy(){
		fecha = System.DateTime.Now;
		actualizarFecha ();
	}
}
//		© NNA-Gamers 2017, Man Sanz
#if UNITY_EDITOR
[CustomEditor(typeof(DatePickerControl))]
public class DatePickerControl_Editor : Editor {

	public override void OnInspectorGUI(){
		DrawDefaultInspector();

		DatePickerControl script = (DatePickerControl)target;

		if (script.formato == DatePickerControl.nFormato.custom) {
			EditorGUILayout.PrefixLabel ("Format Custom");
			script.formatoCustom = EditorGUILayout.TextArea (script.formatoCustom);
		} else if (script.formato != DatePickerControl.nFormato.Default) {
			string aux="";
			aux = script.separator.ToString();
			EditorGUILayout.PrefixLabel ("Separator");
			script.separator = EditorGUILayout.TextArea (aux)[0];
		}

		script.dateOn=EditorGUILayout.Toggle("Date On", script.dateOn);

		if(script.dateOn){

			script.inputFieldDate.fontSizeCustom=EditorGUILayout.ToggleLeft
				("Font Size Custom", script.inputFieldDate.fontSizeCustom);

			script.inputFieldDate.day=EditorGUILayout.ObjectField
				("     InputFiel Day", script.inputFieldDate.day, typeof(InputField), true) as InputField;
			if(script.inputFieldDate.fontSizeCustom)
				script.inputFieldDate.fontSizeDay = EditorGUILayout.IntField ("       Font Size Day",
					script.inputFieldDate.fontSizeDay);

			script.inputFieldDate.month=EditorGUILayout.ObjectField
				("     InputFiel Month", script.inputFieldDate.month, typeof(InputField), true) as InputField;
			if(script.inputFieldDate.fontSizeCustom)
				script.inputFieldDate.fontSizeMonth = EditorGUILayout.IntField ("       Font Size Month",
					script.inputFieldDate.fontSizeMonth);

			script.inputFieldDate.year=EditorGUILayout.ObjectField
				("     InputFiel Year", script.inputFieldDate.year, typeof(InputField), true) as InputField;
			if(script.inputFieldDate.fontSizeCustom)
				script.inputFieldDate.fontSizeYear = EditorGUILayout.IntField ("       Font Size Year",
					script.inputFieldDate.fontSizeYear);
			
			EditorGUILayout.Space ();		
		}

		script.timeOn=EditorGUILayout.Toggle("Time On", script.timeOn);

		if(script.timeOn){

			script.inputFieldTime.fontSizeCustom=EditorGUILayout.ToggleLeft
				("Font Size Custom", script.inputFieldTime.fontSizeCustom);

			script.inputFieldTime.hour=EditorGUILayout.ObjectField
				("     InputFiel Hour", script.inputFieldTime.hour, typeof(InputField), true) as InputField;
			if(script.inputFieldTime.fontSizeCustom)
				script.inputFieldTime.fontSizeHour = EditorGUILayout.IntField ("       Font Size Hour",
					script.inputFieldTime.fontSizeHour);

			script.inputFieldTime.minute=EditorGUILayout.ObjectField
				("     InputFiel Minute", script.inputFieldTime.minute, typeof(InputField), true) as InputField;
			if(script.inputFieldTime.fontSizeCustom)
				script.inputFieldTime.fontSizeMinute = EditorGUILayout.IntField ("       Font Size Minute",
					script.inputFieldTime.fontSizeMinute);

			script.inputFieldTime.second=EditorGUILayout.ObjectField
				("     InputFiel Second", script.inputFieldTime.second, typeof(InputField), true) as InputField;
			if(script.inputFieldTime.fontSizeCustom)
				script.inputFieldTime.fontSizeSecond = EditorGUILayout.IntField ("       Font Size Second",
					script.inputFieldTime.fontSizeSecond);
			
			EditorGUILayout.Space ();
		}
	}
}
#endif

/*****
//	▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
//	█  █▄   █ █▄   █ █▀▀▀▀█          █▀▀▀▀▀ █▀▀▀▀█ █▄  ▄█ █▀▀▀▀▀ █▀▀▀▀█ █▀▀▀▀▀   █
//	█  █ ▀▄ █ █ ▀▄ █ █▄▄▄▄█  ▄▄▄▄▄▄  █  ▄▄▄ █▄▄▄▄█ █ ▀▀ █ █▄▄▄▄  █▄▄▄▄█ █▄▄▄▄▄   █
//	█  █   ▀█ █   ▀█ █    █          █▄▄▄▄█ █    █ █    █ █▄▄▄▄▄ █   ▀▄ ▄▄▄▄▄█   █
//	▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
//		© NNA-Gamers 2017, Man Sanz
*****/