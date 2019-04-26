using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

namespace TriggerForwarding
{
    public class TriggerAggregator : MonoBehaviour
    {

        [SerializeField]
        List<GameObject> listeners;

        List<TriggerListener> trueListeners = new List<TriggerListener>();

        // Use this for initialization
        void Start()
        {
            foreach (GameObject g in listeners)
            {
                AddListener(g);
            }

            foreach (Collider c in gameObject.GetComponentsInChildren<Collider>())
            {
                if (c.gameObject.GetInterfaces<TriggerListener>().Length > 0) continue;
                TriggerForwarder tf = c.gameObject.AddComponent<TriggerForwarder>();
                tf.RegisterAggregator(this);
            }
        }

        public void AddListener(GameObject g)
        {
            trueListeners.AddRange(g.GetInterfaces<TriggerListener>());
        }

        public void OnTriggerEnter(Collider other)
        {
            foreach (TriggerListener c in trueListeners)
            {
                c.OnTriggerEnter(other);
            }
        }

        public void OnTriggerExit(Collider other)
        {
            foreach (TriggerListener c in trueListeners)
            {
                c.OnTriggerExit(other);
            }
        }

        public void OnTriggerStay(Collider other)
        {
            foreach (TriggerListener c in trueListeners)
            {
                c.OnTriggerStay(other);
            }
        }
    }

    public class TriggerForwarder : MonoBehaviour
    {
        List<TriggerAggregator> aggregators = new List<TriggerAggregator>();

        public void RegisterAggregator(TriggerAggregator aggregator)
        {
            aggregators.Add(aggregator);
        }

        private void OnTriggerEnter(Collider other)
        {
            foreach (TriggerAggregator ta in aggregators)
            {
                ta.OnTriggerEnter(other);
            }
        }
        private void OnTriggerExit(Collider other)
        {
            foreach (TriggerAggregator ta in aggregators)
            {
                ta.OnTriggerExit(other);
            }
        }
        private void OnTriggerStay(Collider other)
        {
            foreach (TriggerAggregator ta in aggregators)
            {
                ta.OnTriggerStay(other);
            }
        }
    }

    public interface TriggerListener
    {
        void OnTriggerEnter(Collider other);
        void OnTriggerExit(Collider other);
        void OnTriggerStay(Collider other);
    }
}
