using UnityEngine;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.DesignPatterns
{
	public class ObserverPattern <T> : SingletonPattern <T> where T : MonoBehaviour
	{
		#region Properties

		// List which holds reference to all observers
		private List<IObserver>	m_observers 	= new List<IObserver>();

		#endregion

		#region Observer Methods

		/// <summary>
		/// Adds the observer.
		/// </summary>
		/// <param name="_observer">_observer.</param>
		public void AddObserver (IObserver _observer)
		{
			if (!m_observers.Contains(_observer))
			{
				Debug.Log(string.Format("[ObserverPattern] Adding new observer= {0}.", _observer.ToString()));
				m_observers.Add(_observer);
			}
		}

		/// <summary>
		/// Removes the observer.
		/// </summary>
		/// <param name="_observer">_observer.</param>
		public void RemoveObserver (IObserver _observer)
		{
			if (m_observers.Contains(_observer))
			{
				Debug.Log(string.Format("[ObserverPattern] Removing observer= {0}.", _observer.ToString()));
				m_observers.Remove(_observer);
			}
		}

		/// <summary>
		/// Notifies all the listeners.
		/// </summary>
		/// <param name="_key">_key.</param>
		/// <param name="_data">_data.</param>
		public virtual void NotifyObservers (string _key, ArrayList _data)
		{
			Debug.Log(string.Format("[ObserverPattern] {0} is notifying observers with key {1}.", this.ToString(), _key));

			for (int _iter = 0; _iter < m_observers.Count; _iter++)
			{
				m_observers[_iter].OnPropertyChange(_key, _data);
			}	
		}
		
		#endregion
	}
}