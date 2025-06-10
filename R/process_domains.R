# R/process_domains.R
process_all_domains <- function(patient_data) {
  # Define all domains with their configurations
  domain_configs <- list(
    iq = list(
      domain = "Intelligence",
      scales = get_scales_for_domain("iq", patient_data)
    ),
    academics = list(
      domain = "Academic Achievement",
      scales = get_scales_for_domain("academics", patient_data)
    ),
    verbal = list(
      domain = "Verbal",
      scales = get_scales_for_domain("verbal", patient_data)
    ),
    spatial = list(
      domain = "Spatial",
      scales = get_scales_for_domain("spatial", patient_data)
    ),
    memory = list(
      domain = "Memory",
      scales = get_scales_for_domain("memory", patient_data)
    ),
    executive = list(
      domain = "Executive Function",
      scales = get_scales_for_domain("executive", patient_data)
    ),
    motor = list(
      domain = "Motor",
      scales = get_scales_for_domain("motor", patient_data)
    ),
    social = list(
      domain = "Social",
      scales = get_scales_for_domain("social", patient_data)
    ),
    adhd_adult = list(
      domain = "ADHD (Adult)",
      scales = get_scales_for_domain("adhd_adult", patient_data)
    ),
    adhd_child = list(
      domain = "ADHD (Child)",
      scales = get_scales_for_domain("adhd_child", patient_data)
    ),
    emotion_adult = list(
      domain = "Emotion Regulation (Adult)",
      scales = get_scales_for_domain("emotion_adult", patient_data)
    ),
    emotion_child = list(
      domain = "Emotion Regulation (Child)",
      scales = get_scales_for_domain("emotion_child", patient_data)
    ),
    adaptive = list(
      domain = "Adaptive Behavior",
      scales = get_scales_for_domain("adaptive", patient_data)
    ),
    daily_living = list(
      domain = "Daily Living Skills",
      scales = get_scales_for_domain("daily_living", patient_data)
    )
  )

  # Process each domain
  results <- lapply(names(domain_configs), function(pheno) {
    config <- domain_configs[[pheno]]
    process_single_domain(
      pheno = pheno,
      domain = config$domain,
      scales = config$scales,
      patient_data = patient_data
    )
  })

  names(results) <- names(domain_configs)
  return(results)
}
